This app will measure the correlation by ICQ and Pearson's R for each cell in an image. It accepts tif images containing Z-stacks.

It first identifies the cells contained in each image, and registers cells through the stack. It then identifies, for each cell, which slices are in focus.

It then measures the intensity of each channel for each cell in in an image. It assumes the first channel is blue, the second green, and the third red. 

It then calculates the intensity correlation quotient (ICQ) and the Pearson's R between Red and Green, for each cell in the image.

The app returns the mean value for each cell.

The app also allows for a numeric explanatory variable to be recorded. The application accepts as input a folder. Within this folder will be subfolders named according to numerical values. These values will be used to label the data.

Data is written to a csv file in the working directory.




This app is based around the ImageJ Java library. 
Schneider, C.A., Rasband, W.S., Eliceiri, K.W. "NIH Image to ImageJ: 25 years of image analysis". Nature Methods 9, 671-675, 2012.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.