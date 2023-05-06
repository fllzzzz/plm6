<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <el-date-picker
          v-model="query.date"
          type="daterange"
          range-separator=":"
          size="small"
          value-format="x"
          :shortcuts="PICKER_OPTIONS_SHORTCUTS"
          unlink-panels
          start-placeholder="开始日期"
          end-placeholder="结束日期"
          style="width: 240px"
          class="filter-item"
          @change="handleDateChange"
        />
      </template>
      <template #viewLeft>
        <el-tag v-if="props.rowDetail?.levelName" size="medium" effect="plain" type="warning" class="filter-item">
          {{ props.rowDetail?.levelName }}
        </el-tag>
        <common-button v-permission="crud.permission.add" type="primary" size="mini">摊销设置</common-button>
        <common-button v-permission="crud.permission.add" type="warning" size="mini" @click="amortizationSettingVisible = true">
          摊销设置
        </common-button>
      </template>
    </crudOperation>
    <amortization-setting v-model="amortizationSettingVisible" @success="crud.toQuery" />
  </div>
</template>
<script setup>
import { ref, defineProps } from 'vue'

import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import amortizationSetting from './amortization-setting'

const props = defineProps({
  rowDetail: {
    type: Object,
    default: () => {}
  }
})

const defaultQuery = {
  date: [],
  startDate: undefined,
  endDate: undefined,
  ids: [] // 摊销分类的ids
}

const { crud, query } = regHeader(defaultQuery)

const amortizationSettingVisible = ref(false)

function handleDateChange() {
  if (query.date?.length) {
    query.startDate = query.date[0]
    query.endDate = query.date[1]
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}
</script>

<style lang="scss" scoped>
.el-button + .el-button {
  margin-left: 6px;
}
</style>
