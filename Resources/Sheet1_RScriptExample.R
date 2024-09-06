# Title: An example of an R script to create a Mandlebrot fractal 
#
# This script has three purposes:
#     1. Create a Mandlebrot fractal and display it as an image
#     2. Show good practice in writing and structuring an R script
#     3. Style notes on R script formatting  are added
#
# Written: Jon Yearsley (Jon.Yearsley@ucd.ie)
# Date: Aug 2016
#
# ..................
# Style Note: 
#        Always include a header at the start of an R script
#        A header should include a title, short description, author's name, 
#            contact details and date
# ..................
# 
###########################################


# ..................
# Style Note: 
#        Use comments (text following a #) to help explain your commands
#        Aim for each line to have a maximum width of 80 characters 
# ..................



# One command at the start of an R script is:
rm(list=ls())  # Clear R's memory





# ..................
# Style Note: 
#      Using white space to create structure
#      Use comments to create dividers between sections
# ..................






# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Section 1: Settings to control the look of the fractal -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Define Limits of the image 
aLimits=c(-1.5, 0.5)           # Limits for the x-axis
bLimits=c(-1, 1)               # Limits for the y-axis

resolution = 0.005            # Define the resolution of the final image

MaxIter = 25                  # Define maximum number of iterations


# ..................
# Style Note: 
#      Aligning comments vertical creates structure and 
#        makes the script more readable
# ..................






# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Section 2: Calculate the fractal image -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Calculate the resolution of the fractal
aStep=seq(from=aLimits[1], 
          to=aLimits[2], 
          by=resolution)

bStep=seq(from=bLimits[1], 
          to=bLimits[2], 
          by=resolution)


# ..................
# Style Note: 
#      Do split commands over multiple lines if it makes the script clearer
# ..................



# Create a matrix that will contain the fractal image
fractalImage=array(0,
                   dim=c(length(aStep),length(bStep)))


# Loop through every value in aStep and bStep and 
#  calculate the value of the fractal at each point
for(i in 1:length(aStep)) {
  for(j in 1:length(bStep)) 
  {
    x=0;y=0;n=0;Dist=0        # Set x,y and Dist to zero
    while(n<MaxIter & Dist<4)
    {
      n=n+1                   # Count number of iterations
      newx=aStep[i]+x^2-y^2   # Calculate new x value
      newy=bStep[j]+2*x*y     # Calculate new y value
      Dist=newx^2+newy^2      # Calculate distance from origin
      x=newx;y=newy           # Set x and y to new values
    }
    if (Dist<4) {             # If Dist<4 (iteration has converged)
      colour=MaxIter+1        # Set colour to black (value MaxIter+1) 
    } else {
      colour=n                # Else, set it to a number between 1 and MaxIter
                              #    The colour for each number is defined below
    } 

    fractalImage[i,j]= colour # Set the colour for this x=i, y=j pixel.
  }
}








# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Section 3: Display the fractal image -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Define the colours to use in the image 
cl = rainbow(MaxIter+1)       # Pick colours from the rainbow
cl[MaxIter+1] = '#000000'     # Set the last colour to be black

# Display an image of the fractal using the colours defined by cl
image(x=aStep, 
      y=bStep, 
      z=fractalImage, 
      col=cl)




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# End of Script
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
