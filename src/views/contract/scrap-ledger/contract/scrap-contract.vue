<template>
  <common-drawer v-model="visible" :before-close="handleClose" size="100%" title="合同配置" ref="drawerRef">
    <template #titleRight>
      <common-button type="success" size="mini" @click="addList">新增</common-button>
    </template>
    <template #content>
      <common-table :data="tableData" :data-format="dataFormat" :max-height="maxHeight">
        <el-table-column label="序号" align="center" width="100" type="index"></el-table-column>
        <el-table-column label="购买方" align="center" prop="purchaser">
          <template #default="{row}">
            <el-input v-if="row.isEdit" v-model="row.purchaser" />
            <span v-else>{{ row.purchaser }}</span>
          </template>
        </el-table-column>
        <el-table-column label="合同编号" align="center" prop="serialNumber">
          <template #default="{row}">
            <el-input v-if="row.isEdit" v-model="row.serialNumber" />
            <span v-else>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column label="创建人" align="center" width="150" prop="userName"></el-table-column>
        <el-table-column label="创建时间" align="center" prop="createTime"></el-table-column>
        <el-table-column label="操作" align="center">
          <template #default="{row}">
            <div v-if="row.isEdit">
              <common-button type="success" @click="edit(row)">确认</common-button>
              <common-button type="danger" @click="row.isEdit = false">取消</common-button>
            </div>
            <div v-else>
              <common-button icon="el-icon-edit" type="primary" @click="row.isEdit = true" />
              <common-button icon="el-icon-delete" type="danger" @click="delRow(row)" />
            </div>
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
      <add-contract v-model="showDlg" @success="addSuccess" />
    </template>
  </common-drawer>
</template>
<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import { getScrapContractList, editScrapContractList, delScrapContractList } from '@/api/contract/scrap-ledger'
import { ElMessage } from 'element-plus'
import useMaxHeight from '@compos/use-max-height'
import addContract from './add-contract.vue'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  }
})

const emit = defineEmits(['update:modelValue'])

const dataFormat = ref([['createTime', ['parse-time', '{y}-{m}-{d}']]])

const { visible, handleClose } = useVisible({ props, emit, showHook: fetchData })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchData })

const showDlg = ref(false)
const tableData = ref([])
const drawerRef = ref()

async function fetchData() {
  tableData.value = []
  try {
    const { content, totalElements } = await getScrapContractList({ ...queryPage })
    setTotalPage(totalElements)
    console.log(content)
    content.forEach(v => {
      v.isEdit = false
    })
    tableData.value = content
  } catch (error) {
    console.log(error)
  }
}

const { maxHeight } = useMaxHeight(
  {
    // mainBox: '.product-detail',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    paginate: true,
    extraHeight: 120,
    minHeight: 300,
    navbar: false
  },
  drawerRef
)

function addList() {
  showDlg.value = true
}

function addSuccess() {
  fetchData()
}

async function edit(row) {
  const _list = []
  const obj = {
    purchaser: row.purchaser,
    id: row.id,
    serialNumber: row.serialNumber
  }
  _list.push(obj)
  try {
    await editScrapContractList(_list)
    ElMessage({ message: '修改成功', type: 'success' })
    row.isEdit = false
    fetchData()
  } catch (error) {
    console.log(error, '配置修改失败')
  }
}

async function delRow(row) {
  const ids = []
  ids.push(row.id)
  try {
    await delScrapContractList(ids)
    ElMessage({ message: '删除成功', type: 'success' })
    fetchData()
  } catch (error) {
    console.log(error, '配置删除失败')
  }
}

</script>
