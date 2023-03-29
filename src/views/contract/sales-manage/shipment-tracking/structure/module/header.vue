<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.name"
        placeholder="可输入名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.material"
        placeholder="输入材质搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optRight>
        <el-tag size="medium" effect="plain">
          统计日期：
          <span v-parse-time="{ val: statisticalTime?.[0], fmt: '{y}-{m}-{d}' }" />
          ~
          <span v-parse-time="{ val: statisticalTime?.[1], fmt: '{y}-{m}-{d}' }" />
        </el-tag>
      </template>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="contractStructurePrice"
          :params="{ projectId: query.projectId }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { watch, nextTick, inject } from 'vue'

import { convertUnits } from '@/utils/convert/unit'
import { toThousand } from '@/utils/data-type/number'
import { emptyTextFormatter } from '@/utils/data-type'
import { pricingMannerEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const projectId = inject('projectId')
const statisticalTime = inject('statisticalTime')

watch(
  projectId,
  (val) => {
    nextTick(() => {
      crud.query.projectId = val
      crud.toQuery()
    })
  },
  { immediate: true }
)

const defaultQuery = {
  name: undefined,
  material: undefined,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v) => {
    v.totalWeight = convertUnits(v.totalWeight, 'kg', 't', DP.COM_WT__T)
    v.newUnitPrice = v.unitPrice // number类型的单价（unitPrice可能会有千位符）
    v.originNewUnitPrice = v.newUnitPrice
    v.originUnitPrice = emptyTextFormatter(toThousand(v.unitPrice))
    v.totalPrice = v.pricingManner === pricingMannerEnum.WEIGHT.V ? v.totalWeight * (v.unitPrice || 0) : v.totalLength * (v.unitPrice || 0)
    v.originPricingManner = v.pricingManner
  })
}
</script>
