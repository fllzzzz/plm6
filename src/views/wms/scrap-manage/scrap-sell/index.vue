<template>
  <div class="app-container">
    <mHeader />
    <common-table :data="crud.data" style="width: 100%;" :data-format="dataFormat">
      <el-table-column label="序号" width="60" align="center" type="index" />
      <el-table-column label="出售日期" width="150" align="center" prop="saleDate" />
      <el-table-column label="购买单位" width="200" align="center" prop="purchaser" />
      <el-table-column label="合同编号" width="200" align="center" prop="serialNumber" />
      <el-table-column label="总金额" width="120" align="center" prop="totalAmount" />
      <el-table-column label="创建人" width="100" align="center" prop="createUserName" />
      <el-table-column label="创建日期" width="150" align="center" prop="createTime" />
      <el-table-column label="审核人" width="100" align="center" prop="auditUserName" />
      <el-table-column label="审核日期" width="150" align="center" prop="auditTime" />
      <el-table-column label="审核状态" width="120" align="center" fixed="right">
        <template #default="{row}">
          <el-tag size="medium" :type="row.auditStatusEnum === auditTypeEnum.AUDITING.V ? 'info' : row.auditStatusEnum === auditTypeEnum.PASS.V ? 'success' : 'danger'">{{ auditTypeEnum.VL[row.auditStatusEnum] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="操作" width="220" align="center" fixed="right">
        <template #default="{row}">
          <common-button type="primary" icon="el-icon-view" size="mini" @click="showView(row)" v-permission="crud.permission.get" />
          <common-button type="success" icon="el-icon-edit" size="mini" :disabled="row.auditStatusEnum === auditTypeEnum.PASS.V" @click="showDetail(row)" v-permission="crud.permission.edit" />
          <common-button type="warning" icon="el-icon-s-check" size="mini" :disabled="row.auditStatusEnum === auditTypeEnum.PASS.V" @click="showCheck(row)" v-permission="crud.permission.check" />
          <common-button type="danger" icon="el-icon-delete" size="mini" :disabled="row.auditStatusEnum === auditTypeEnum.PASS.V" @click="delItem(row)" v-permission="crud.permission.del" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页组件 -->
    <pagination />
    <!-- 审核详情 -->
    <check-detail v-model="checkVisible" :rowData="currentRow" @success="checkSuccess" />
    <viewDetail v-model="viewVisible" :rowData="currentRow" />
    <detail :title="'修改废料出售'" :rowData="detailRow" v-model="detailVisible" @success="editSuccess"  />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import crudApi, { delScrapSell } from '@/api/wms/scrap-sell'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header.vue'
import pagination from '@crud/Pagination'
import checkDetail from './module/check-detail.vue'
import viewDetail from './module/view-detail.vue'
import { auditTypeEnum } from '@enum-ms/contract'
import { ElMessage } from 'element-plus'
import detail from './module/detail.vue'
import { scrapSellPM as permission } from '@/page-permission/wms'

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['saleDate', ['parse-time', '{y}-{m}-{d}']],
  ['auditTime', ['parse-time', '{y}-{m}-{d}']]
])

const { crud } = useCRUD({
  title: '废料出售',
  crudApi: { ...crudApi },
  sort: [],
  permission: { ...permission }
})

const checkVisible = ref(false)
const viewVisible = ref(false)
const currentRow = ref({})
const detailRow = ref({})
const detailVisible = ref(false)

function showCheck(row) {
  checkVisible.value = true
  currentRow.value = row
}

function checkSuccess() {
  crud.toQuery()
}

function showView(row) {
  viewVisible.value = true
  currentRow.value = row
}

function showDetail(row) {
  detailVisible.value = true
  detailRow.value = row
}

function editSuccess() {
  crud.toQuery()
}

async function delItem(row) {
  const delList = []
  delList.push(row.id)
  try {
    await delScrapSell(delList)
    crud.toQuery()
    ElMessage({ message: '删除成功', type: 'success' })
  } catch (error) {
    console.log(error)
  }
}

</script>
