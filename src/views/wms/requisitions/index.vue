<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      @selection-change="crud.selectionChangeHandler"
      row-key="id"
    >
      <el-table-column type="selection" width="55" align="center" />
        <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('purchaseNo')" key="purchaseNo" prop="purchaseNo" :show-overflow-tooltip="true" label="申购单号" min-width="180" />
      <el-table-column
        v-if="columns.visible('basicClassNames')"
        key="basicClassNames"
        :show-overflow-tooltip="true"
        prop="basicClassNames"
        label="物料种类"
        min-width="170px"
      />
      <el-table-column
        v-if="columns.visible('project')"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="250"
      >
        <template #default="{ row }">
          <span class="project-name">{{ projectNameFormatter(row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('writtenByName')" key="writtenByName" prop="writtenByName" :show-overflow-tooltip="true" label="填写人" min-width="120" />
      <el-table-column v-if="columns.visible('purchaseOrderNos')" key="purchaseOrderNos" prop="purchaseOrderNos" :show-overflow-tooltip="true" label="关联采购单号" min-width="160" />
      <el-table-column
        v-if="columns.visible('status')"
        key="status"
        label="采购状态"
        prop="status"
        align="center"
        width="90px"
      >
        <template #header>
          <el-tooltip
            class="item"
            effect="light"
            :content="`采购执行完毕后，该采购单不可在入库办理处选择。\n
            已结算的采购单不可再打开采购状态。`"
            placement="top"
          >
            <div style="display: inline-block">
              <span>采购状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <el-switch
            v-if="checkPermission(permission.editStatus)"
            v-model="row.status"
            :disabled="
              row.enabledLoading ||
              (row.status === settlementStatusEnum.SETTLED.V && row.status === purchaseStatusEnum.FINISHED.V)
            "
            active-color="#13ce66"
            :active-value="purchaseStatusEnum.UNFINISHED.V"
            :inactive-value="purchaseStatusEnum.FINISHED.V"
            @change="handleEnabledChange(row, 'purchaseNo')"
          />
          <el-tag v-else :type="row.status">{{ purchaseStatusEnum.VL[row.status] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('founderName')"
        key="founderName"
        :show-overflow-tooltip="true"
        prop="founderName"
        label="创建人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('lastOperatorName')"
        key="lastOperatorName"
        :show-overflow-tooltip="true"
        prop="lastOperatorName"
        label="最后操作人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('userUpdateTime')"
        key="userUpdateTime"
        :show-overflow-tooltip="true"
        prop="userUpdateTime"
        label="编辑日期"
        align="center"
        width="100"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.userUpdateTime }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="230px" align="center" fixed="right">
        <template #default="{ row }">
          <e-operation :data="row.id" :permission="permission.download" />
          <udOperation :disabled-edit="row.status == purchaseStatusEnum.FINISHED.V" :data="row" show-detail />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 表单 -->
    <m-form />
    <m-detail />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/wms/requisitions'
import { ref } from 'vue'
import EO from '@enum'
import { settlementStatusEnum } from '@enum-ms/finance'
import { purchaseStatusEnum } from '@enum-ms/wms'
import { matClsEnum } from '@/utils/enum/modules/classification'
import checkPermission from '@/utils/system/check-permission'
import { projectNameFormatter } from '@/utils/project'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import pagination from '@crud/Pagination'
import eOperation from '@crud/E.operation'
import udOperation from '@crud/UD.operation.vue'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'

const permission = {
  get: ['wms_requisitions:get'],
  add: ['wms_requisitions:add'],
  edit: ['wms_requisitions:edit'],
  editStatus: ['wms_requisitions:editStatus'],
  download: ['wms_requisitions:download'],
  del: ['wms_requisitions:del']
}

const optShow = {
  add: true,
  edit: true,
  del: true,
  download: false
}

const tableRef = ref()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '物料申购订单',
    sort: ['id.desc'],
    invisibleColumns: ['userUpdateTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    detailFormApi: false,
    formStore: true,
    formStoreKey: 'WMS_REQUISITIONS'
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
const { handleEnabledChange } = useCrudEnabledChange(
  { CRUD, crud, editEnabled: editStatus },
  { enabledField: 'status', enumObj: purchaseStatusEnum, t: 'UNFINISHED', f: 'FINISHED' }
)

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.content.map((v) => {
    if (!v.project) {
      v.project = { shortName: '公共备料' }
    }
    const basicClassArr = EO.getBits(matClsEnum.ENUM, v.basicClass, 'L')
    v.basicClassNames = basicClassArr.join(' | ')
    if (v.purchaseOrderNoList) {
      v.purchaseOrderNos = v.purchaseOrderNoList.join('、')
    }
    return v
  })
}

</script>
