<template>
  <div class="project-chart">
      <div class="chart-head">
          <span>设备任务完成量统计</span>
          <el-date-picker
            v-model="year"
            type="year"
            size="small"
            :clearable="false"
             style="width: 150px !important"
            placeholder="选择年"
            value-format="YYYY"
            format="YYYY"
            :disabled-date="disabledDate"
            @change="handleYearChange"
        />
        </div>
      <div class="chart-container">
        <!-- <el-row  v-loading="projectInfo.loading" v-permission="crud.permission.statistics" :gutter="20" class="panel-group"> -->
        <el-row :gutter="20" class="panel-group">
            <el-col :span="12" class="card-panel-col">
                <panel name="设备台数" num-color="#1890ff" />
            </el-col>
            <el-col :span="12" class="card-panel-col">
            <!-- <panel name="全部套料" num-color="#1890ff" :end-val="projectInfo.summary.nestingNum || 0" /> -->
                <panel name="当年累计切割量（吨）"   num-color="#1890ff" />
            </el-col>
        </el-row>
        </div>
    <el-divider border-style="double" />
  </div>
  <div class="line-chart">
       <div class="chart-head">
          <span style="color:red">年度切割量：1253.36t</span>
          <el-date-picker
            v-model="year1"
            type="year"
            size="small"
            :clearable="false"
             style="width: 150px !important"
            placeholder="选择年"
            value-format="YYYY"
            format="YYYY"
            :disabled-date="disabledDate"
            @change="handleYearChange"
        />
        </div>
        <lineChart/>
         <el-divider border-style="double" />
  </div>
  <div class="column-chart">
       <div class="chart-head">
          <span style="color:red">月度切割量：1253.36t</span>
          <el-date-picker
            v-model="month"
            type="month"
            size="small"
            :clearable="false"
            style="width: 150px !important"
            placeholder="选择月"
            value-format="YYYY/MM"
            format="YYYY/MM"
            class="filter-item"
            :disabled-date="disabledDate"
            @change="handleYearChange"
        />
        </div>
        <columnChart/>
  </div>
</template>

<script setup>
import { ref, defineEmits, onMounted } from 'vue'
import { parseTime } from '@/utils/date'
import Panel from '@/components/Panel'
import lineChart from './module/lineChart.vue'
import columnChart from './module/columnChart.vue'

const emit = defineEmits(['update:year', 'update:year1', 'update:month'])
const year  = ref(parseTime(new Date(), '{y}'))
const year1 = ref(parseTime(new Date(), '{y}'))
const month = ref(parseTime(new Date(), '{y}-{m}'))

onMounted(() => {
  // 默认选择当年
  handleYearChange(year.value)
  // 默认选择当年
  handleYearChange(year1.value)
  //默认选择当月
  handleMonthChange(month.value)
})
function disabledDate(time) {
  return time > new Date()
}
function handleYearChange(val) {
  emit('update:year', val)
  emit('update:year1', val)
}
function handleMonthChange(val) {
  emit('update:month', val)
}
</script>

<style lang="scss" scoped>
.chart-container {
    padding-top: 10px;
}
    .project-chart {
        flex: 0.5;
        .chart-head {
            display: flex;
            justify-content: space-between;
        }
        span {
            font-size: 14px;
        }
    }
     .line-chart {
        flex: 1;
        .chart-head {
            display: flex;
            justify-content: space-between;
        }
        span {
            font-size: 14px;
        }
    }
     .column-chart {
        flex: 1;
        .chart-head {
            display: flex;
            justify-content: space-between;
        }
        span {
            font-size: 14px;
        }
    }
</style>
