#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb  2 21:25:19 2021

@author: obin
"""
import matplotlib.pyplot as plt
import numpy as np
# Creating timing figures for the masters thesis Advecting Superspecies



def section1_bar():
    """Plot highest level timing estimates for runs with and without VBS"""
    runs = [[7024.28, 2497.41  ],
    [ 13095.98 , 3935.69  ]]
    labels = ["Parallel Run", "Single Domain"]
    X = np.arange(2)
    width = 0.28
    fig, ax = plt.subplots()
    ax.bar(X+width/2, runs[0], color = 'b', width = 0.25, label = 'No VBS')
    ax.bar(X+1.5*width, runs[1], color = 'r', width = 0.25,label='VBS')
    ax.set_xticks(X+width)
    ax.set_xticklabels( ['24 Subdomain Run', 'Single Domain Run'] )
    ax.set_ylabel("System clock [seconds]")
    ax.legend()
    plt.show()
    return

section1_bar()



def section2_pie(novbs,vbs,title,novbstime,vbstime):
    """More granular reporting of parallel and sequential runs"""
    labels = ["Timestep Setup", "Chemistry", "Vertical Diffusion", 
                       "Advection", "Emissions and Other < 1%", "Sedimentation and Deposition"]

    explode = (0, 0.0, 0, 0.0, 0, 0)
    colors = ['y','r','g','c','b','m']
    colors = ['m','y','b','g','r','c']
    
    fig1, (ax1, ax2) = plt.subplots(1,2,figsize=(12, 7))
    ax1.pie(novbs, explode=explode, autopct='%1.1f%%',
            shadow=False, colors = colors, startangle=90)
    ax1.axis('equal')  
    ax1.set_title('No VBS: ~' + str(novbstime) + ' seconds',fontsize = 18.0)  
    
    ax2.pie(vbs, explode=explode, autopct='%1.1f%%',
            shadow=False, colors = colors, startangle=90)
    ax2.axis('equal')  
    ax2.set_title('VBS: ~' + str(vbstime) + ' seconds',fontsize = 18.0)
    plt.legend(labels,bbox_to_anchor=(0.22,0.17))
    plt.suptitle(title,fontsize = 25.0)
    plt.show()
    return fig1
    
seq_vbs = [11.4, 28.7, 11.1, 20.6, 0.4, 27.8]
seq_novbs = [19.5, 34.7, 8.9, 21.5, 0.3, 15.1]
section2_pie(seq_novbs,seq_vbs,"Sequential Run",2500,3900)


par_vbs = [8.0, 14.6, 9.6, 57.0, 0.8, 10.0]
par_novbs = novbs = [12.1, 20.3, 8.6, 50.2, 0.3, 8.5,]
section2_pie(par_novbs,par_vbs,"Parallel Run",7000,13100)



def section6_pie(novbs,vbs,superspecies,title,novbstime,vbstime,superspeciestime):
    """More granular reporting of parallel and sequential runs"""
    labels = ["Timestep Setup", "Chemistry", "Vertical Diffusion", 
                       "Advection", "Emissions and Other < 2%", "Sedimentation and Deposition"]

    explode = (0, 0.0, 0, 0.0, 0, 0)
    colors = ['mediumorchid','y','b','g','r','c']
    
    fig1, (ax1, ax2, ax3) = plt.subplots(1,3,figsize=(15, 7))
    ax1.pie(novbs, explode=explode, autopct='%1.1f%%',
            shadow=False, colors = colors, startangle=270)
    ax1.axis('equal')  
    ax1.set_title('No VBS: ~' + str(novbstime) + ' seconds',fontsize = 18.0)  
    
    ax2.pie(vbs, explode=explode, autopct='%1.1f%%',
            shadow=False, colors = colors, startangle=270)
    ax2.axis('equal')  
    ax2.set_title('VBS: ~' + str(vbstime) + ' seconds',fontsize = 18.0)
    
    ax3.pie(superspecies, explode=explode, autopct='%1.1f%%',
            shadow=False, colors = colors, startangle=270)
    ax3.axis('equal')  
    ax3.set_title('Superspecies: ~' + str(superspeciestime) + ' seconds',fontsize = 18.0)
    
    plt.legend(labels,bbox_to_anchor=(0.22,0.17))
    plt.suptitle(title,fontsize = 25.0)
    plt.show()
    return fig1
    
seq_novbs_MACC = [37.6, 25.1, 4.1, 20.2, 0.5, 12.5]  
seq_vbs_MACC = [16.1, 28.1, 5.2, 30.2, 1.6, 18.8]
seq_superspecies_MACC = [20.7, 29.9, 6.2, 19.9, 0.8 , 22.5]
seq_macc = section6_pie(seq_novbs_MACC,seq_vbs_MACC,seq_superspecies_MACC,"Sequential Run on MACC Domain",22985,40031,34192)  
plt.savefig('figs/seq_macc_timing.png', format='png')

novbs_CAMS = [12.4, 20.8, 5.6, 46.0, 1.8, 13.4]
vbs_CAMS = [6.3, 16.9, 5.6, 53.1, 1.3, 16.8]
superspecies_CAMS = [5.7, 20.0, 7.8, 41.5, 1.5 , 23.5]
CAMS = section6_pie(novbs_CAMS,vbs_CAMS,superspecies_CAMS,"Parallel Run on CAMS Domain",75055,140691,119251)  
plt.savefig('figs/cams_timing.png', format='png')


par_novbs_MACC = [37.6, 25.1, 4.1, 20.2, 0.5, 12.5]  
par_vbs_MACC = [16.1, 28.1, 5.2, 30.2, 1.6, 18.8]
par_superspecies_MACC = [20.7, 29.9, 6.2, 19.9, 0.8 , 22.5]
section6_pie(seq_novbs_MACC,seq_vbs_MACC,seq_superspecies_MACC,"Parallel Run on MACC Domain, 8 subdomains",22985,40031,34192)   


def advection_bar():
    """Plot highest level timing estimates for runs with and without VBS"""
    runs = [[4638.53, 34958.76  ],
    [ 12073.01 , 74762.33  ],
    [ 6789.65, 49472.83 ]]
    #labels = ["Parallel Run", "Single Domain"]
    X = np.arange(2)
    width = 0.25
    fig, ax = plt.subplots()
    ax.bar(X, runs[0], color = 'b', width = 0.25, label = 'No VBS', edgecolor = "black")
    ax.bar(X+width, runs[1], color = 'r', width = 0.25,label='VBS', edgecolor = "black")
    ax.bar(X+2*width, runs[2], color = 'g', width = 0.25,label='Superspecies', edgecolor = "black")
    ax.set_xticks(X+width)
    ax.set_xticklabels( ['MACC Domain', 'CAMS Domain'] )
    ax.set_ylabel("System clock [seconds]")
    ax.set_title("Timing of Advection Operator")
    ax.legend()
    plt.show()
    return

advection_bar()
plt.savefig('figs/advection_bar.pdf')