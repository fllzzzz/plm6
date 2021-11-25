<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.productType"
        :options="shipAuditStatusEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.checkStatus"
        :options="packTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <div>
        <el-input
          v-model="query.serialNumber"
          placeholder="可输入车次搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.licensePlate"
          placeholder="可输入车牌搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.userName"
          placeholder="可输入装车人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.actualUserName"
          placeholder="可输入过磅人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </div>
    <crudOperation>
      <template v-slot:optLeft>
        <!-- <export-button
          v-permission="permission.downloadLogistics"
          :params="{...query}"
          :fn="downloadLogistics"
          btn-text="下载物流汇总表（根据查询条件下载）"
          class="filter-item"
        />
        <print-table
          v-permission="[...permission.print, ...permission.detailPrint]"
          v-model:current-key="currentKey"
          :api-key="apiKey"
          :params="printParams"
          :before-print="handleBeforePrint"
          size="mini"
          type="warning"
          class="filter-item"
        /> -->
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { inject, onMounted, ref } from 'vue'

import { packTypeEnum, shipAuditStatusEnum } from '@enum-ms/mes'
// import { isNotBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  serialNumber: undefined,
  licensePlate: undefined,
  userName: undefined,
  actualUserName: undefined,
  projectId: { value: undefined, resetAble: false },
  productType: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

const permission = inject('permission')
// const currentKey = ref()
const apiKey = ref([])

onMounted(() => {
  if (checkPermission(permission.print)) {
    apiKey.value.push('STEEL_MES_PACK_SHIP')
  }
  if (checkPermission(permission.detailPrint)) {
    apiKey.value.push('STEEL_MES_PACK_SHIP_DETAIL')
  }
})

// const printParams = computed(() => {
//   if (currentKey.value === 'STEEL_MES_PACK_SHIP') {
//     return { ...query }
//   }
//   if (currentKey.value === 'STEEL_MES_PACK_SHIP_DETAIL' && isNotBlank(crud.selections)) {
//     return crud.selections.map(row => {
//       return row.id
//     })
//   }
//   return undefined
// })

// function handleBeforePrint() {
//   if (currentKey.value === 'STEEL_MES_PACK_SHIP_DETAIL' && !isNotBlank(printParams)) {
//     $message.warning('至少选择一条需要打印的发运信息')
//     return false
//   }
// }
</script>
