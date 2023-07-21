<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-if="crud.searchToggle">
          <el-date-picker
            v-model="month"
            :editable="false"
            size="small"
            type="monthrange"
            value-format="x"
            format="YYYY-MM"
            range-separator=":"
            class="filter-item"
            start-placeholder="开始月份"
            end-placeholder="结束月份"
            :clearable="false"
            :disabledDate="time => time.getTime() > Date.now()"
            style="width: 200px"
            @change="crud.toQuery"
          />
          <material-cascader
            v-model="query.classifyId"
            separator=" > "
            check-strictly
            show-all-levels
            clearable
            multiple
            size="small"
            class="filter-item"
            style="width: 300px"
            placeholder="可选择/输入科目、编号搜索"
            @change="crud.toQuery"
          />
          <rrOperation />
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref } from 'vue'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import { specFormat, specTip } from '@/utils/wms/spec-format'

import { setSpecInfoToList } from '@/utils/wms/spec'
import moment from 'moment'

const defaultTime = [moment().startOf('month').valueOf().toString(), moment().format('x')]

const month = ref(defaultTime)

const defaultQuery = {
  times: defaultTime, // [开始月份，结束月份]
  classifyId: undefined
}

const { crud, query, CRUD } = regHeader(defaultQuery)

CRUD.HOOK.beforeResetQuery = () => {
  month.value = defaultTime
}

// el-date-picker type="monthrange"时， default-time 不生效
// 转月末时间戳
CRUD.HOOK.beforeToQuery = () => {
  query.times = [month.value[0], moment(parseInt(month.value[1])).endOf('month').valueOf()]
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  // 设置规格等信息，多规格模式
  await setSpecInfoToList(data.content, { multipleSpec: true })
  for (const row of data.content) {
    // --------------------- 处理规格 -------------------------
    row.multipleSpec = Array.isArray(row.specifications) && row.specifications.length > 1
    // 规格格式转换
    const formatSpecArr = []
    const fmtObj = {
      basicClass: row.basicClass, // 基础分类
      thickness: row.thickness, // 厚度
      width: row.width, // 宽度
      length: row.length, // 长度
      color: row.color // 颜色
    }
    if (Array.isArray(row.specifications)) {
      for (const spec of row.specifications) {
        fmtObj.specification = spec // 规格
        const specFmt = specFormat(fmtObj)
        formatSpecArr.push(specFmt)
      }
    } else {
      const specFmt = specFormat(fmtObj)
      formatSpecArr.push(specFmt)
    }

    row.formatSpecArr = formatSpecArr
    row.sourceFormatSpecArr = [...formatSpecArr]
    // 规格提示信息
    row.specTip = specTip({ ...fmtObj, specificationLabels: row.specificationLabels })
  }
}
</script>
