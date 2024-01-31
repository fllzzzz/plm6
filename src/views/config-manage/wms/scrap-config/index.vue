<template>
  <div class="app-container">
    <div class="head-container">
      <common-button icon="el-icon-plus" type="success" @click="addScrap" v-permission="permission.add">新增</common-button>
    </div>
    <common-table :data="tableData">
      <el-table-column label="序号" align="center" width="80" type="index" />
      <el-table-column label="废料类型" align="center" prop="name"></el-table-column>
      <el-table-column label="单位" align="center" prop="measureUnit"></el-table-column>
      <el-table-column label="操作" align="center">
        <template #default="{row}">
          <common-button v-permission="permission.edit" type="primary" @click="editScrap(row)">修改</common-button>
          <common-button v-permission="permission.del" type="danger" @click="delScrap(row)">删除</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页组件 -->
    <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    <detaile v-model="detaileVisible" @success="success" :rowData="rowData" :title="detaileTitle" />
  </div>
</template>
<script setup>
import { onMounted, ref } from 'vue'
import { getScrapConfig, delScrapType } from '@/api/config/wms/scrap-config'
import { configWmsScrapConfigPM as permission } from '@/page-permission/config'
import detaile from './module/detaile.vue'
// import { ElMessage } from 'element-plus'
import usePagination from '@compos/use-pagination'
import checkPermission from '@/utils/system/check-permission'

const detaileVisible = ref(false)
const rowData = ref({})
const tableData = ref([])
const detaileTitle = ref('创建废料类型')

onMounted(() => {
  fetchData()
})
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchData })

async function fetchData() {
  if (!checkPermission(permission.get)) return
  tableData.value = []
  try {
    const { content, totalElements } = await getScrapConfig({ ...queryPage })
    tableData.value = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log(error)
  }
}

const addScrap = () => {
  detaileTitle.value = '创建废料类型'
  detaileVisible.value = true
}

const editScrap = (row) => {
  detaileTitle.value = '修改废料类型'
  rowData.value = row
  detaileVisible.value = true
}

const success = () => {
  fetchData()
}

const delScrap = async (row) => {
  const _delList = []
  _delList.push(row.id)
  try {
    await delScrapType(_delList)
    // ElMessage({ message: '删除成功', type: 'success' })
    fetchData()
  } catch (error) {
    console.log(error)
  }
}

</script>
