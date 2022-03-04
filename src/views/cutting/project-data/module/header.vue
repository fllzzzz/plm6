<template>
  <div class="head-container">
    <!-- <div v-if="crud.searchToggle"> -->
      <!-- <el-input
        v-model.trim="query.serialNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按项目名称搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.operatorName"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按单体名称搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.operatorName"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按区域名称搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      /> -->
 <!-- <el-input
        clearable
        style="width: 200px"
        size="small"
        placeholder="按项目名称搜索"
        class="filter-item"
      />
      <el-input
        clearable
        style="width: 200px"
        size="small"
        placeholder="按单体名称搜索"
        class="filter-item"
       />
       <el-input
        clearable
        style="width: 200px"
        size="small"
        placeholder="按区域名称搜索"
        class="filter-item"
       /> -->
       <el-input
        v-model.trim="a"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按项目名称搜索"
        class="filter-item"
        />
       <el-input
        v-model.trim="a"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按厚度搜索"
        class="filter-item"
       />
      <el-input
        v-model.trim="a"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按物料种类搜索"
        class="filter-item"
      />
      <!-- <rrOperation /> -->
    <!-- </div> -->
    <!-- <crudOperation /> -->
  </div>
</template>

<script setup>
const a = undefined
// import { ref } from 'vue'
// import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
// import { purchaseStatusEnum, baseMaterialTypeEnum } from '@enum-ms/wms'

// import { regHeader } from '@compos/use-crud'
// import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
// import rrOperation from '@crud/RR.operation'
// import crudOperation from '@crud/CRUD.operation'
// import supplierSelect from '@comp-base/supplier-select/index.vue'

// const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

// const defaultQuery = {
//   createTime: [], // [开始日期，结束日期]
//   purchaseType: undefined, // 采购类型
//   purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
//   projectId: { value: undefined, resetAble: false }, // 项目id
//   serialNumber: undefined, // 采购单号搜索
//   supplierId: undefined, // 供应商id
//   operatorName: undefined, // 创建人 or 最后操作人
// }

// const { crud, query } = regHeader(defaultQuery)
// useGlobalProjectIdChangeToQuery(crud)
</script>
