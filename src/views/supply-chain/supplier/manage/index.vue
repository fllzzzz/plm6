<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      return-source-data
      :show-empty-symbol="false"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      @selection-change="crud.selectionChangeHandler"
      row-key="id"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        :show-overflow-tooltip="true"
        prop="name"
        min-width="150"
        label="供应商名称"
      />
      <el-table-column v-if="columns.visible('area')" key="area" :show-overflow-tooltip="true" prop="area" label="地区" min-width="150" />
      <el-table-column
        v-if="columns.visible('supplierClassification')"
        key="supplierClassification"
        max-width="320"
        :show-overflow-tooltip="true"
        prop="supplierClassification"
        label="供应商分类"
      />
      <el-table-column align="center">
        <template #header>
          <el-tooltip class="item" effect="light" content="被隐藏后，无法在订单编辑中搜索到该供应商" placement="top">
            <span>列入隐藏列表 <i class="el-icon-info" /></span>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <el-switch
            v-model="row.enabled"
            :disabled="!checkPermission(permission.editStatus)"
            active-color="#ff4949"
            inactive-color="#909399"
            :active-value="supplierIsHideEnum.TRUE.V"
            :inactive-value="supplierIsHideEnum.FALSE.V"
            @change="changeEnabled(row, row.enabled)"
          />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间" width="140px" />
      <!--编辑与删除-->
      <el-table-column
        label="操作"
        width="180px"
        align="center"
        fixed="right"
      >
        <template #default="{ row }">
          <udOperation show-detail :data="{ id: row.id }" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
    <mDetail />
    <m-batch-form />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/supply-chain/supplier/manage'
import { supplierPM as permission } from '@/page-permission/supply-chain'

import { ref } from 'vue'
import { supplierClassEnum, supplierIsHideEnum } from '@enum-ms/supplier'
import { getLabelByBit } from '@/utils/enum/base'
import { parseTime } from '@/utils/date'
import checkPermission from '@/utils/system/check-permission'
import { useStore } from 'vuex'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import mBatchForm from './module/batch-form'

const store = useStore()

import { ElMessageBox } from 'element-plus'

const optShow = {
  batchAdd: true,
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '供应商',
    sort: ['id.desc'],
    formStore: true,
    formStoreKey: 'SUPPLY_CHAIN_SUPPLIER_MANAGE',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

async function changeEnabled(data, val) {
  try {
    await ElMessageBox.confirm(
      `此操作将"${supplierIsHideEnum.VL[val]}"${data.name}\n${
        val === supplierIsHideEnum.TRUE.V ? '被隐藏后，无法在订单编辑中搜索到该供应商\n' : ''
      } 是否继续？`,
      '提示',
      {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }
    )
    await editStatus({ id: data.id, boolHide: val })
    crud.refresh()
    crud.notify(supplierIsHideEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('操作供应商状态', error)
    data.enabled = !data.enabled
  }
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.createTime = parseTime(v.createTime)
    v.supplierClassification = getLabelByBit(supplierClassEnum, parseInt(v.supplierClassification), '、')
    return v
  })
}

function handleSuccess() {
  store.dispatch('config/fetchSuppliers')
}

CRUD.HOOK.afterSubmit = () => {
  handleSuccess()
}

CRUD.HOOK.afterBatchAddSuccess = () => {
  handleSuccess()
}

CRUD.HOOK.afterDelete = () => {
  handleSuccess()
}

CRUD.HOOK.beforeToAdd = (crud, data) => {
  crud.form.processType = crud.query.processType
  crud.form.sequenceType = crud.query.sequenceType
}
</script>
