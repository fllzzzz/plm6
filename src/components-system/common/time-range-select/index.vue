<!-- 年月日:时间范围选择 -->
<template>
  <div class="time-range-select">
    <common-select
      v-model="queryVO.dateQueryFormat"
      :options="dateQueryTypeEnum.ENUM"
      type="enum"
      :size="props.size"
      @change="handleChange"
    />
    <!-- 年 -->
    <el-date-picker
      v-if="queryVO.dateQueryFormat === dateQueryTypeEnum.YEAR.V"
      v-model="year"
      type="year"
      :size="props.size"
      placeholder="选择年"
      format="YYYY"
      :clearable="props.clearable"
      value-format="YYYY"
      class="time-range"
      :disabled-date="disabledDateFn"
      @change="handleChange"
    />
    <!-- 月 -->
    <el-date-picker
      v-else-if="queryVO.dateQueryFormat === dateQueryTypeEnum.MONTH.V"
      v-model="month"
      type="month"
      :size="props.size"
      placeholder="选择月"
      :clearable="props.clearable"
      format="YYYY-MM"
      value-format="x"
      class="time-range"
      :disabled-date="disabledDateFn"
      @change="handleChange"
    />
    <!-- 日 -->
    <el-date-picker
      v-else
      v-model="day"
      type="daterange"
      range-separator=":"
      value-format="x"
      class="time-range"
      unlink-panels
      :size="props.size"
      :clearable="props.clearable"
      :disabled-date="disabledDateFn"
      :default-time="defaultTime"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      start-placeholder="开始时间"
      end-placeholder="结束时间"
      @change="handleChange"
    />
  </div>
</template>

<script setup>
import { defineProps, defineEmits, watchEffect, ref, nextTick } from 'vue'

import { dateQueryTypeEnum } from '@enum-ms/contract'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import moment from 'moment'

const emit = defineEmits(['change'])
const props = defineProps({
  query: {
    type: Object,
    default: () => {
      return {
        dateQueryFormat: undefined,
        startDate: undefined,
        endDate: undefined
      }
    }
  },
  size: {
    type: String,
    default: 'small'
  },
  clearable: {
    type: Boolean,
    default: false
  },
  disabledDateFn: {
    type: Function,
    default: (time) => time > new Date()
  }
})

const year = ref(moment().year().toString())
const month = ref(moment().startOf('month').format('x'))
const day = ref([moment().startOf('day').format('x'), moment().format('x')])
const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const queryVO = ref({})
watchEffect(() => {
  queryVO.value = props.query
  if (props.query.dateQueryTypeEnum) {
    queryVO.value.dateQueryFormat = props.query.dateQueryTypeEnum
  } else {
    queryVO.value.dateQueryFormat = dateQueryTypeEnum.YEAR.V
  }
  nextTick(() => {
    handleChange()
  })
})

function handleChange() {
  if (queryVO.value.dateQueryFormat === dateQueryTypeEnum.YEAR.V && year.value) {
    queryVO.value.startDate = moment(year.value).startOf('year').format('x')
    queryVO.value.endDate = moment(year.value).endOf('year').format('x')
  } else if (queryVO.value.dateQueryFormat === dateQueryTypeEnum.MONTH.V && month.value) {
    queryVO.value.startDate = moment(+month.value).startOf('month').format('x')
    queryVO.value.endDate = moment(+month.value).endOf('month').format('x')
  } else if (queryVO.value.dateQueryFormat === dateQueryTypeEnum.DAY.V && day.value?.length) {
    queryVO.value.startDate = day.value[0]
    queryVO.value.endDate = day.value[1]
  } else {
    queryVO.value.startDate = undefined
    queryVO.value.endDate = undefined
  }
  emit('change')
}
</script>

<style lang="scss" scoped>
.time-range-select {
  > .el-select {
    width: 50px;
    ::v-deep(.el-input__inner) {
      padding-left: 8px;
      padding-right: 0;
      border-top-right-radius: 0;
      border-bottom-right-radius: 0;
    }
  }
  > ::v-deep(.time-range) {
    width: calc(100% - 50px);
    border-top-left-radius: 0;
    border-bottom-left-radius: 0;
    .el-input__inner {
      border-top-left-radius: 0;
      border-bottom-left-radius: 0;
    }
  }
}
</style>
