<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
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
      >
        <template #default="{ row }">
          <span>{{ row.supplierClassification }}</span>
        </template>
      </el-table-column>
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
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="180px"
        align="center"
        fixed="right"
      >
        <template #default="{ row }">
          <udOperation :show-detail="true" :data="{ id: row.id }" />
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
import crudApi, { editStatus } from '@/api/wms/supplier/manage'
import { ref } from 'vue'
import { ElMessageBox } from 'element-plus'

import { supplierClassEnum, supplierIsHideEnum } from '@enum-ms/supplier'
import { getLabelByBit } from '@/utils/enum/base'
import { parseTime } from '@/utils/date'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import mBatchForm from './module/batch-form'

// crud交由presenter持有
const permission = {
  get: ['wms_supplier:get'],
  add: ['wms_supplier:add'],
  edit: ['wms_supplier:edit'],
  del: ['wms_supplier:del'],
  editStatus: ['wms_supplier:editStatus'],
  downloadAttachments: ['wms_supplier:downloadAttachments']
}

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
    formStoreKey: 'WMS_SUPPLIER_MANAGE',
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
    console.log('操作构件工序状态', error)
    data.boolHide = data.boolHide === supplierIsHideEnum.FALSE.V ? supplierIsHideEnum.TRUE.V : supplierIsHideEnum.FALSE.V
  }
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.createTime = parseTime(v.createTime)
    v.supplierClassification = getLabelByBit(supplierClassEnum, parseInt(v.supplierClassification), '、')
    return v
  })
}

CRUD.HOOK.beforeToAdd = (crud, data) => {
  crud.form.processType = crud.query.processType
  crud.form.sequenceType = crud.query.sequenceType
}
</script>
