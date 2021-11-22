<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="!loaded || crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="serialNumber"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="expand">
        <template #header>
          <el-icon @click="handleExpandAll"><el-arrow-down v-if="expandAll" /><el-arrow-right v-else /></el-icon>
        </template>
        <template #default="scope">
          <div class="table-expand-container">
            <p>关联项目：<span v-empty-text>{{ scope.row.projectStr }}</span></p>
            <p>关联申购单：<span v-empty-text>{{ scope.row.requisitionsSNStr }}</span></p>
          </div>
        </template>
      </el-table-column>
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60">
        <template #default="scope">
          <table-cell-tag :show="scope.row.supplyType == orderSupplyTypeEnum.PARTY_A.V" name="甲供" :color="TAG_PARTY_DEF_COLOR" />
          <span>{{ scope.$index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="编制日期"
        align="center"
        width="100"
      >
        <template #default="scope">
          <table-cell-tag
            v-if="scope.row.settlementStatus == settlementStatusEnum.SETTLED.V"
            :name="settlementStatusEnum.SETTLED.L"
            :color="settlementStatusEnum.SETTLED.COLOR"
          />
          <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.createTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="订单号"
        min-width="160px"
      />
      <el-table-column
        v-if="columns.visible('typeText')"
        key="typeText"
        :show-overflow-tooltip="true"
        prop="typeText"
        label="物料种类"
        min-width="170px"
      />
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" label="关联项目" min-width="170px">
        <template #default="scope">
          <el-tooltip effect="light" placement="top">
            <template #content>
              <div v-for="item in scope.row.projects" :key="item.id" class="project-name" style="margin-bottom: 5px">
                {{ projectNameFormatter(item, undefined, false) }}
              </div>
            </template>
            <div style="overflow: hidden; text-overflow: ellipsis; white-space: nowrap">
              <span v-for="item in scope.row.projects" :key="item.id"> 【{{ item.shortName }}】 </span>
            </div>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('supplier.name')"
        key="supplier.name"
        :show-overflow-tooltip="true"
        prop="supplier.name"
        label="供应商"
        min-width="150"
      />
      <el-table-column
        v-if="columns.visible('mete')"
        key="mete"
        :show-overflow-tooltip="true"
        prop="mete"
        label="合同量"
        align="center"
        width="110"
      >
        <template #default="scope">
          <span>{{ scope.row.mete ? `${scope.row.mete} ${scope.row.meteUnit}` : '' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('amount')"
        key="amount"
        :show-overflow-tooltip="true"
        prop="amount"
        label="合同额（元）"
        align="center"
        width="110"
      />
      <el-table-column
        v-if="columns.visible('invoiceType')"
        key="invoiceType"
        :show-overflow-tooltip="true"
        prop="invoiceType"
        label="票据类型"
        align="center"
        min-width="130"
      >
        <template #default="scope">
          <span v-parse-enum="{e:invoiceTypeEnum, v:scope.row.invoiceType}"/>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('purchaseStatus')"
        key="purchaseStatus"
        label="采购状态"
        prop="purchaseStatus"
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
        <template #default="scope">
          <el-switch
            v-if="checkPermission(permission.editPurchaseStatus)"
            v-model="scope.row.purchaseStatus"
            :disabled="
              scope.row.enabledLoading ||
              (scope.row.status === settlementStatusEnum.SETTLED.V && scope.row.purchaseStatus === purchaseStatusEnum.FINISHED.V)
            "
            active-color="#13ce66"
            :active-value="purchaseStatusEnum.UNFINISHED.V"
            :inactive-value="purchaseStatusEnum.FINISHED.V"
            @change="handleEnabledChange(scope.row, 'serialNumber')"
          />
          <el-tag v-else :type="scope.row.purchaseStatus">{{ purchaseStatusEnum.VL[scope.row.purchaseStatus] }}</el-tag>
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
      <!--编辑与删除-->
      <el-table-column label="操作" width="230px" align="center" fixed="right">
        <template #default="scope">
          <e-operation :data="scope.row.id" :permission="permission.download" />
          <udOperation :disabled-edit="scope.row.purchaseStatus == purchaseStatusEnum.FINISHED.V" :data="scope.row" show-detail />
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
import crudApi, { editPurchaseStatus } from '@/api/wms/purchase-order'
import { ref, computed } from 'vue'
import EO from '@enum'
import { invoiceTypeEnum, settlementStatusEnum } from '@enum-ms/finance'
import { orderSupplyTypeEnum, purchaseStatusEnum, baseMaterialTypeEnum } from '@enum-ms/wms'
import { materialClassificationEnum } from '@/utils/enum/modules/classification'
import checkPermission from '@/utils/system/check-permission'
import { TAG_PARTY_DEF_COLOR } from '@/settings/config'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import useSuppliers from '@compos/store/use-suppliers'
import pagination from '@crud/Pagination'
import eOperation from '@crud/E.operation'
import udOperation from '@crud/UD.operation.vue'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'
import { projectNameFormatter } from '@/utils/project'

// crud交由presenter持有
const permission = {
  get: ['wms_purchaseOrder:get'],
  add: ['wms_purchaseOrder:add'],
  edit: ['wms_purchaseOrder:edit'],
  editPurchaseStatus: ['wms_purchaseOrder:editPurchaseStatus'],
  download: ['wms_purchaseOrder:download'],
  del: ['wms_purchaseOrder:del']
}

const optShow = {
  add: true,
  edit: true,
  del: true,
  download: false
}

const tableRef = ref()
const expandRowKeys = ref([])
const expandAll = computed(() => expandRowKeys.value.length === crud.data.length)

const { CRUD, crud, columns } = useCRUD(
  {
    title: '物料采购订单',
    sort: ['id.desc'],
    invisibleColumns: ['invoiceType', 'updateTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { loaded, supplierKV } = useSuppliers()
const { maxHeight } = useMaxHeight({ paginate: true })
const { handleEnabledChange } = useCrudEnabledChange(
  { CRUD, crud, editEnabled: editPurchaseStatus },
  { enabledField: 'purchaseStatus', enumObj: purchaseStatusEnum, t: 'UNFINISHED', f: 'FINISHED' }
)

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.content.map((v) => {
    const basicClassArr = EO.getBits(materialClassificationEnum.ENUM, v.basicClass, 'L')
    v.typeText = baseMaterialTypeEnum.VL[v.purchaseType] + ' - ' + basicClassArr.join(' | ')
    v.supplier = computed(() => supplierKV.value[v.supplierId])
    v.requisitionsSNStr = v.requisitionsSN.join('　、　')
    v.projectStr = v.projects && v.projects.map((v) => projectNameFormatter(v, null, false)).join('　、　')
    v.projectId = v.projects.map(v => v.id)
    return v
  })
}

// 展开所有行
function handleExpandAll() {
  if (!expandAll.value) {
    expandRowKeys.value = crud.data.map((v) => v.serialNumber)
  } else {
    expandRowKeys.value = []
  }
}
</script>
