    !MIT License
    !
    !Copyright (c) [2017] [HydroGeophysics Group, Aarhus University, Denmark]
    !
    !Permission is hereby granted, free of charge, to any person obtaining a copy
    !of this software and associated documentation files (the "Software"), to deal
    !in the Software without restriction, including without limitation the rights
    !to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    !copies of the Software, and to permit persons to whom the Software is
    !furnished to do so, subject to the following conditions:
    !
    !The above copyright notice and this permission notice shall be included in all
    !copies or substantial portions of the Software.
    !
    !THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    !IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    !FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    !AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    !LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    !OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    !SOFTWARE.
    
    !Simple example that shows the OpenMp parallelization framework in action.
    program Parallelization_Example
    use mParallel
    use mMath
    implicit none
    integer :: AffinityMode,NCPUs,UseNested,StartThread,NCPUsLow,NCPUOuter,AffinityPattern
    integer :: n,i
    real*8, allocatable :: x(:),y(:),z(:)
    
    !Settings for the parallelization framework
    AffinityMode=1         !Dedicated server
    NCPUs = -1             !We want to use all except one
    UseNested=0            !We do not want nested parallelization
    !The rest we let the program handle automatically
    StartThread=-99        !Which threadID to start the affinity binding to 
    NCPUsLow=-99           !Number of CPU's to use in a memory bandwidth limited region (In this case this is used inside the dot product routine.)
    NCPUOuter=-99          !Only used in nested parallelization  
    AffinityPattern=-99    !Should the affinity be set as scattered or compact?
    
    !Before we do anything else openMp related, we call initOpenMp(), this initializes the parallelization framework
    call InitOpenMP(AffinityMode,AffinityPattern,StartThread,UseNested,NCPUs,NCPUsLow,NCPUOuter)

    !Now lets create 3 vectors to work on
    n=100000000
    allocate(x(n))
    allocate(y(n))
    allocate(z(n))
    
    print*,'Starting first dot product, for this one we have no outer parallelization, but only rely on the parallelization in the dotproduct routine, which we have chosen to be memory bandwidth limited.'
    print*,'Because the dot product is memory bandwidth limited in this example it uses NCPUsLow number of threads'
    x(:)=10
    y(:)=3.9123
    !We start by calling the dot_product routine
    do i=1,100 !let's put it in a loop just so we can see what happens
      z=SMatdot_real(n,x,y)
    end do
    print*,'First dot product done,starting second'
    !Because no parallelization was detected while running SMatdot_real it ran the dot product in parallel, due to the way parallelization is called with StartOpenMp.
    
    !Now lets do the same, but this time with parallelization already on
    print*,'Starting second dot product, for this one we have outer parallelization, hence the inner parallelization will detect this outer parallelization and not be used.'
    print*,'Because we only run the dot_product on the Master node, only 1 thread will be working during this dot product.'
    !$OMP PARALLEL IF(StartOpenMP(1)) PRIVATE(I) DEFAULT(SHARED)      
    !$OMP MASTER !We make sure only 1 thread enters this region
      do i=1,100 !let's put it in a loop just so we can see what happens
        z=SMatdot_real(n,x,y)
      end do
      !Because previous parallelization was detected, this region does not run with additional parallelization. Had we enabled nested parallelization and had we called the outer region with StartOpenMp(3) then we could still have gotten the dot product routine parallelized.
    !$OMP END MASTER
    !$OMP END PARALLEL    
    end program Parallelization_Example

