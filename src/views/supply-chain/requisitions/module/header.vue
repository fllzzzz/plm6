<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.boolInitiateApprove"
        :options="approvalEnum"
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.materialType"
        :options="materialPurchaseClsEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-if="query.boolInitiateApprove"
        v-model="query.status"
        :options="ddReviewStatusEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.enabled"
        :options="enabledEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.times"
        type="daterange"
        :default-time="defaultTime"
        range-separator=":"
        size="small"
        value-format="x"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        unlink-panels
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 240px"
        class="filter-item"
        :disabledDate="
          (date) => {
            return date.getTime() > new Date().getTime()
          }
        "
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="可输入申购编号搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation addText="申购创建" />
  </div>
</template>

<script setup>
import { watch, ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { ddReviewStatusEnum } from '@enum-ms/dd'
import { materialPurchaseClsEnum } from '@enum-ms/classification'
import { enabledEnum } from '@enum-ms/common'

import useApprovalCfg from '@compos/store/use-approval-cfg'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const approvalEnum = {
  FALSE: { L: '无审批', K: 'FALSE', V: false },
  TRUE: { L: '有审批', K: 'TRUE', V: true }
}

const defaultQuery = {
  times: [], // [开始日期，结束日期]
  boolInitiateApprove: { value: approvalEnum.FALSE.V, resetAble: false },
  status: undefined,
  serialNumber: undefined,
  materialType: undefined
}
const { crud, query } = regHeader(defaultQuery)

const { approvalCfg, loaded } = useApprovalCfg()

// 监听审批配置是否加载完成
watch(
  () => loaded.value,
  (val) => {
    if (val) {
      query.boolInitiateApprove = approvalCfg.value.requisition
      crud.toQuery()
    }
  },
  { immediate: true }
)
</script>
