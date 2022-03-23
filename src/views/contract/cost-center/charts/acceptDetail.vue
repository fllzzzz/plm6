<template>
  <div :id="id" :style="{width, height}" />
</template>

<script>
import echarts from 'echarts'
import resize from '@/mixins/resize'
require('echarts/theme/macarons')
export default {
  mixins: [resize],
  props: {
    totalAmount: {
      type: Number,
      default: 0
    },
    payMsg: {
      type: Array,
      default: () => []
    },
    id: {
      type: String,
      default: 'chart'
    },
    width: {
      type: String,
      default: '100%'
    },
    height: {
      type: String,
      default: '600px'
    }
  },
  data() {
    return {
      ydata: [],
      xdata: [],
      chart: null
    }
  },
  watch: {
    payMsg() {
      this.initData()
    }
  },
  mounted() {
    this.initData()
  },
  beforeDestroy() {
    if (this.chart) {
      this.chart.dispose()
      this.chart = null
    }
  },
  methods: {
    initData() {
      this.xdata = []
      this.ydata = []
      for (var j = 0; j < this.payMsg.length; j++) {
        this.ydata.push((this.payMsg[j].amount / this.totalAmount))
        this.xdata.push(this.payMsg[j].type)
      }
      this.initChart(this.xdata, this.ydata)
    },
    initChart(xdata, ydata) {
      this.chart = echarts.init(document.getElementById(this.id))
      this.chart.setOption({
        title: {
          text: '支出分析',
          textStyle: {
            lineHeight: 50,
            color: '#008ACD'
          }
        },
        tooltip: {
          trigger: 'axis',
          formatter: (params) => {
            var temp = `${params[0].seriesName} <br/> ${((params[0].value || 0) * (this.totalAmount || 0)).toFixed(2)}元`
            return temp
          },
          axisPointer: {
            type: 'shadow'
          },
          crossStyle: {
            color: '#555'
          }
        },
        grid: {
          left: '50px',
          right: '50px',
          borderWidth: 0,
          top: 150,
          bottom: 95,
          textStyle: {
            color: '#fff'
          }
        },
        legend: {
          position: 'center',
          top: '5%',
          textStyle: {
            color: '#34bfa3'
          },
          data: ['总金额']
        },
        calculable: true,
        xAxis: [{
          type: 'category',
          axisPointer: {
            type: 'shadow'
          },
          axisLine: {
            show: true,
            symbol: ['none', 'arrow'],
            lineStyle: {
              color: '#008ACD'
            }
          },
          splitLine: {
            show: false
          },
          axisLabel: {
            interval: 0,
            rotate: 0
          },
          data: xdata
        }],
        yAxis: [{
          show: true,
          type: 'value',
          name: '占比',
          // 坐标轴在 grid 区域中的分隔线。
          max: 1,
          min: 0,
          splitLine: {
            show: true
          },
          splitArea: {
            show: true
          },
          axisLabel: {
            interval: 0,
            formatter: '{value}'
          },
          axisTick: {
            show: true
          },
          axisLine: {
            show: true,
            symbol: ['none', 'arrow'],
            lineStyle: {
              color: '#008ACD'
            }
          }
        }],
        //   系列数据
        series: [
          {
            name: '总金额',
            type: 'bar',
            itemStyle: {
              normal: {
                color: '#34bfa3',
                label: {
                  show: true,
                  position: 'top',
                  textStyle: {
                    color: 'black'
                  },
                  formatter: function(p) {
                    return p.value > 0 ? (p.value * 100).toFixed(2) + '%' : ''
                  }
                }
              }
            },
            barMaxWidth: '70px',
            data: ydata
          }
        ],
        optionToContent: function(opt) {
          var axisData = opt.xAxis[0].data
          var series = opt.series
          var table = '<table style="width:100%;text-align:center" cellspacing="0" cellpadding="0" class="table_Qushi"><tbody><tr>' +
                      '<td>序号</td>' +
                      '<td>' + series[0].name + '</td>' +
                      '<td>' + series[1].name + '</td>' +
                      '<td>' + series[2].name + '</td>' +
                      '<td>' + series[3].name + '</td>' +
                      '<td>' + series[4].name + '</td>' +
                      '</tr>'
          for (var i = 0, l = axisData.length; i < l; i++) {
            table += '<tr>' +
                      '<td>' + axisData[i] + '</td>' +
                      '<td>' + series[0].data[i] + '</td>' +
                      '<td>' + series[1].data[i] + '</td>' +
                      '<td>' + series[2].data[i] + '</td>' +
                      '<td>' + series[3].data[i] + '</td>' +
                      '<td>' + series[4].data[i] + '</td>' +
                      '</tr>'
          }
          table += '</tbody></table>'
          return table
        }
      }, true)
    }
  }
}
</script>
