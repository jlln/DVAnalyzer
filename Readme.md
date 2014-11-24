This app will measure the correlation by Pearson's R for each nucleus in an image. It also measures the sizes and numbers of substructures in the red and green channels. It accepts tif images containing Z-stacks, and expects the channels to be in Red-Green-Blue order. It also requires that the images be spatially correlated.

It first identifies the nuclei contained in each image (using the blue channel), and tracks the nuclei through the stack. It then identifies, for each nucleus, which slices are in focus.

It then measures the intensity of each channel for each nucleus in in an image. It assumes the first channel is blue, the second green, and the third red. 

It then calculates the intensity correlation quotient (ICQ) and the Pearson's R between Red and Green, for each nucleus in the image.

The app returns the mean value for each nucleus.

The app also allows for a numeric explanatory variable to be recorded. The application accepts as input a folder. Within this folder will be subfolders named according to explanatory values. These values will be used to label the data.

Data is written to a csv file in the working directory.




This app is based around the ImageJ Java library. 
Schneider, C.A., Rasband, W.S., Eliceiri, K.W. "NIH Image to ImageJ: 25 years of image analysis". Nature Methods 9, 671-675, 2012.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
