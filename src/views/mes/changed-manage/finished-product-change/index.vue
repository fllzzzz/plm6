<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
      <common-table ref="tableRef" :data="crud.data" style="width: 100%;" v-loading="crud.loading" :summary-method="getSummaries" show-summary>
      <el-table-column label="项目信息" align="center" width="500">
        <el-table-column label="序号" align="center" width="50" type="index" :show-overflow-tooltip="true" />
        <el-table-column label="单体" align="center" width="225" prop="monomer.name" :show-overflow-tooltip="true" v-if="columns.visible('monomer.name')" />
        <el-table-column label="区域" align="center" width="225" prop="area.name" :show-overflow-tooltip="true" v-if="columns.visible('area.name')" />
      </el-table-column>
      <el-table-column label="变更前" align="center" width="850">
        <el-table-column label="编号" align="center" width="150" prop="serialNumber" :show-overflow-tooltip="true" v-if="columns.visible('serialNumber')" />
        <el-table-column label="规格" align="center" width="200" prop="specification" :show-overflow-tooltip="true" v-if="columns.visible('specification')">
        <template #default="{row}">
          <span>{{ row.specification }}</span>
          <table-cell-tag v-if="row.newQuantity === 0" name="删" color="#f56c6c"></table-cell-tag>
          <table-cell-tag v-else-if="row.oldQuantity !== row.newQuantity || row.newNetWeight !== row.oldNetWeight || row.totalNewNetWeight !== row.totalOldNetWeight" name="改"></table-cell-tag>
        </template>
        </el-table-column>
        <el-table-column label="材质" width="100" prop="material" :show-overflow-tooltip="true" v-if="columns.visible('material')" />
        <el-table-column label="长度" width="100" prop="length" :show-overflow-tooltip="true" v-if="columns.visible('length')" />
        <el-table-column label="数量" width="100" prop="oldQuantity" :show-overflow-tooltip="true" v-if="columns.visible('oldQuantity')" />
        <el-table-column label="净重" width="100" prop="oldNetWeight" :show-overflow-tooltip="true" v-if="columns.visible('oldNetWeight')" />
        <el-table-column label="总重" width="100" prop="totalOldNetWeight" :show-overflow-tooltip="true" v-if="columns.visible('totalOldNetWeight')" />
      </el-table-column>
      <el-table-column label="变更后" align="center" width="850">
        <el-table-column label="编号" align="center" width="150" prop="serialNumber" :show-overflow-tooltip="true" v-if="columns.visible('serialNumber')">
          <template #default="{row}">
            <span>{{row.newQuantity === 0 ? '-' : row.serialNumber}}</span>
          </template>
        </el-table-column>
        <el-table-column label="规格" align="center" width="200" prop="specification" :show-overflow-tooltip="true" v-if="columns.visible('specification')">
          <template #default="{row}">
            <span>{{row.newQuantity === 0 ? '-' : row.specification}}</span>
          </template>
        </el-table-column>
        <el-table-column label="材质" width="100" prop="material" :show-overflow-tooltip="true" v-if="columns.visible('material')">
          <template #default="{row}">
            <span>{{row.newQuantity === 0 ? '-' : row.material}}</span>
          </template>
        </el-table-column>
        <el-table-column label="长度" width="100" prop="length" :show-overflow-tooltip="true" v-if="columns.visible('length')">
          <template #default="{row}">
            <span>{{row.newQuantity === 0 ? '-' : row.length}}</span>
          </template>
        </el-table-column>
        <el-table-column label="数量" width="100" prop="newQuantity" :show-overflow-tooltip="true" v-if="columns.visible('newQuantity')">
          <template #default="{row}">
            <span v-if="row.oldQuantity === row.newQuantity">{{row.newQuantity === 0 ? '-' : row.newQuantity}}</span>
            <span v-else style="color: red;">{{row.newQuantity === 0 ? '-' : row.newQuantity}}</span>
          </template>
        </el-table-column>
        <el-table-column label="净重" width="100" prop="newNetWeight" :show-overflow-tooltip="true" v-if="columns.visible('newNetWeight')">
          <template #default="{row}">
            <span v-if="row.oldNetWeight === row.newNetWeight">{{row.newQuantity === 0 ? '-' : row.newNetWeight}}</span>
            <span v-else style="color: red;">{{row.newQuantity === 0 ? '-' : row.newNetWeight}}</span>
          </template>
        </el-table-column>
        <el-table-column label="总重" width="100" prop="totalNewNetWeight" :show-overflow-tooltip="true" v-if="columns.visible('totalNewNetWeight')">
          <template #default="{row}">
            <span v-if="row.totalNewNetWeight === row.totalOldNetWeight">{{row.newQuantity === 0 ? '-' : row.totalNewNetWeight}}</span>
            <span v-else style="color: red;">{{row.newQuantity === 0 ? '-' : row.totalNewNetWeight}}</span>
          </template>
        </el-table-column>
      </el-table-column>
    </common-table>
    <!-- 分页组件 -->
    <pagination />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import mHeader from './module/header.vue'
import useCRUD from '@compos/use-crud'
import crudApi from '@/api/mes/changed-manage/finished-product-change'
import pagination from '@crud/Pagination'
import { tableSummary } from '@/utils/el-extra'

// const headerStyle = ref({})
// const projectStyle = ref({})
// const beforeChangeStyle = ref({})
// const afterChangeStyle = ref({})
const tableRef = ref()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, columns } = useCRUD(
  {
    title: '成品变更清单',
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const getSummaries = (param) => {
  const summary = tableSummary(param, {
    props: ['oldQuantity', 'oldNetWeight', 'totalOldNetWeight', 'newQuantity', 'newNetWeight', 'totalNewNetWeight']
  })
  return summary
}

// 样式切换页面不显示滞后处理
// const saveHeaderStyle = () => {
//   const projectStyle = document.querySelector('.el-table thead.is-group th.el-table_1_column_1.el-table__cell')
//   console.log(projectStyle.style)
//   if (projectStyle) {
//     headerStyle.value = {
//       ...projectStyle.style
//     }
//   }
//   console.log(headerStyle.value)
// }

// onBeforeUnmount(() => {
//   saveHeaderStyle()
// })

// onActivated(() => {
//   const projectStyle = document.querySelector('.el-table thead.is-group th.el-table_1_column_1.el-table__cell')
//   if (projectStyle) {
//     Object.assign(projectStyle.style, headerStyle.value)
//   }
// })

</script>

<style lang="scss" scoped>
  ::v-deep(.el-table thead.is-group th.el-table__cell){
    background: #fff;
    color: black;
  }
  ::v-deep(.el-table thead.is-group tr:first-child th.el-table__cell:first-child){
    background: rgb(33, 150, 243);
    color: white;
    font-size: 16px;
  }
::v-deep(.el-table thead.is-group tr:first-child th.el-table__cell:nth-child(2)){
  background: rgb(103, 194, 58);
  color: white;
  font-size: 16px;
}
::v-deep(.el-table thead.is-group tr:first-child th.el-table__cell:nth-child(3)){
  background: rgb(252, 209, 6);
  color: white;
  font-size: 16px;
}
</style>
