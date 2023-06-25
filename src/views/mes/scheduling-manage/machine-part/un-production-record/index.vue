<template>
  <common-drawer
    ref="drawerRef"
    title="零件剔除记录"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
    custom-class="machine-part-un-product-record"
  >
    <template #titleRight> </template>
    <template #content>
      <m-header @toQuery="toQuery" />
      <common-table v-loading="tableLoading" :data="tableData" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" prop="project" label="所属项目" min-width="120px" align="left" />
        <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体" min-width="100px" align="left" />
        <el-table-column :show-overflow-tooltip="true" prop="area.name" label="区域" min-width="100px" align="left" />
        <el-table-column :show-overflow-tooltip="true" prop="artifactSerialNumber" label="构件编号" min-width="100px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="workshop.name" label="车间" min-width="100px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="productionLine.name" label="生产线" min-width="100px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="groups.name" label="班组" min-width="100px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="零件编号" min-width="100px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="quantity" label="零件数量" min-width="100px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="零件规格" min-width="100px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="material" label="零件材质" min-width="100px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="askCompleteTime" label="构件完成日期" width="120px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="user.name" label="操作人" min-width="100px" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="createTime" label="操作时间" width="120px" align="center" />
        <el-table-column label="操作" width="150px" align="center">
          <template #default="{ row: { sourceRow: row } }">
            <common-button size="mini" type="primary" icon="el-icon-edit" @click="toEdit(row)" />
            <common-button size="mini" type="success" icon="el-icon-refresh-left" @click="toRecall(row)"> 撤回 </common-button>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
  <edit-form v-model:visible="editVisible" :info="itemInfo" @refresh="handleRefresh" />
</template>

<script setup>
import { getUnProductRecord, editUnProduct } from '@/api/mes/scheduling-manage/machine-part'
import { defineProps, defineEmits, ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import useVisible from '@compos/use-visible'

import { ElMessageBox, ElNotification } from 'element-plus'
import editForm from './module/edit-form.vue'
import mHeader from './module/header.vue'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false,
  },
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchData, closeHook })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchData })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.machine-part-un-product-record',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    paginate: true,
    minHeight: 300,
  },
  drawerVisible
)

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['askCompleteTime', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project'],
])

const editVisible = ref(false)
const tableLoading = ref(false)
const tableData = ref([])
const query = ref({})
const itemInfo = ref({})
const needRefresh = ref(false)

async function fetchData() {
  try {
    tableLoading.value = true
    const { content = [], totalElements } = await getUnProductRecord({ ...query.value, ...queryPage })
    tableData.value = content
    setTotalPage(totalElements)
  } catch (error) {
    console.error(error)
  } finally {
    tableLoading.value = false
  }
}

function toQuery(params) {
  query.value = { ...params }
  handleRefresh()
}

function handleRefresh() {
  queryPage.pageNumber = 1
  fetchData()
}

function closeHook() {
  if (needRefresh.value) {
    emit('refresh')
  }
}

// 编辑
function toEdit(row) {
  itemInfo.value = row
  editVisible.value = true
}

// 撤回
async function toRecall(row) {
  try {
    await ElMessageBox.confirm(`是否对零件编号${row.serialNumber}进行撤回操作？\n零件数量：${row.quantity}`, '提示', { type: 'warning' })
    await editUnProduct({ id: row.id, quantity: 0 })
    ElNotification({ type: 'success', title: '撤回成功', duration: 2000 })
    handleRefresh()
    needRefresh.value = true
  } catch (error) {
    console.error(error)
  }
}
</script>
