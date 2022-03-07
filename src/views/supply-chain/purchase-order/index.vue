<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="tableLoading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="serialNumber"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="serialNumber">
        <template #default="{ row }">
          <p v-if="isNotBlank(row.auxMaterialIds)">辅材明细：<span v-split="row.auxMaterialNames" /></p>
          <p>关联项目：<span v-parse-project="{ project: row.projects }" v-empty-text /></p>
          <p>
            关联申购单：<span v-empty-text>{{ row.requisitionsSNStr }}</span>
          </p>
          <p>
            备注：<span v-empty-text>{{ row.remark }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60">
        <template #default="{ row, $index }">
          <table-cell-tag :show="row.boolPartyA" name="甲供" type="partyA" />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('branchCompany.name')"
        key="branchCompany.name"
        :show-overflow-tooltip="true"
        prop="branchCompany.name"
        label="签订主体"
        min-width="170px"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="编制日期"
        align="center"
        width="100"
      >
        <template #default="{ row }">
          <table-cell-tag
            v-if="row.settlementStatus == settlementStatusEnum.SETTLED.V"
            :name="settlementStatusEnum.SETTLED.L"
            :color="settlementStatusEnum.SETTLED.COLOR"
          />
          <span v-parse-time="{ val: row.createTime, fmt: '{y}-{m}-{d}' }" />
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
      <el-table-column
        v-if="columns.visible('projects')"
        show-overflow-tooltip
        key="projects"
        prop="projects"
        label="关联项目"
        min-width="170"
      >
        <template #default="{ row }">
          <span v-parse-project="{ project: row.projects, onlyShortName: true }" v-empty-text />
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
        <template #default="{ row }">
          <span>{{ row.mete ? `${row.mete || ''} ${row.meteUnit || ''}` : '' }}</span>
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
        <template #default="{ row }">
          <span v-parse-enum="{ e: invoiceTypeEnum, v: row.invoiceType }" />
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
        <template #default="{ row }">
          <el-switch
            v-if="checkPermission(permission.editPurchaseStatus)"
            v-model="row.purchaseStatus"
            :disabled="
              row.enabledLoading || (row.status === settlementStatusEnum.SETTLED.V && row.purchaseStatus === purchaseStatusEnum.FINISHED.V)
            "
            active-color="#13ce66"
            :active-value="purchaseStatusEnum.UNFINISHED.V"
            :inactive-value="purchaseStatusEnum.FINISHED.V"
            @change="handleEnabledChange(row, 'serialNumber')"
          />
          <el-tag v-else :type="row.purchaseStatus">{{ purchaseStatusEnum.VL[row.purchaseStatus] }}</el-tag>
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
        label="编辑时间"
        align="center"
        width="100"
      >
        <template #default="{ row }">
          <span v-parse-time="row.userUpdateTime" />
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="230px" align="center" fixed="right">
        <template #default="{ row }">
          <e-operation :data="row.id" :permission="permission.download" />
          <udOperation
            :disabled-edit="row.purchaseStatus == purchaseStatusEnum.FINISHED.V"
            :data="row"
            show-detail
            :before-to-detail="beforeToDetail"
            :before-to-edit="beforeToEdit"
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 表单 -->
    <m-raw-material-form v-if="crud.props.formType === 'rawMaterial'" />
    <m-raw-material-detail v-if="crud.props.detailType === 'rawMaterial'"/>
  </div>
</template>

<script setup>
import crudApi, { editPurchaseStatus } from '@/api/supply-chain/purchase-order'
import { purchaseOrderPM as permission } from '@/page-permission/supply-chain'

import { ref, computed } from 'vue'
import EO from '@enum'
import { invoiceTypeEnum, settlementStatusEnum } from '@enum-ms/finance'
import { orderSupplyTypeEnum, purchaseStatusEnum, baseMaterialTypeEnum } from '@enum-ms/wms'
import { matClsEnum } from '@/utils/enum/modules/classification'
import checkPermission from '@/utils/system/check-permission'
import { isNotBlank } from '@/utils/data-type'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import pagination from '@crud/Pagination'
import eOperation from '@crud/E.operation'
import udOperation from '@crud/UD.operation.vue'
import mHeader from './module/header'
import mRawMaterialForm from './module/form/raw-material.vue'
import mRawMaterialDetail from './module/detail/raw-material.vue'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import useMatClsList from '@/composables/store/use-mat-class-list'

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const expandRowKeys = ref([])

const { CRUD, crud, columns } = useCRUD(
  {
    title: '采购订单',
    sort: ['id.desc'],
    invisibleColumns: ['branchCompany.name', 'invoiceType', 'userUpdateTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: [],
    // detailFormApi: false,
    formStore: true,
    queryable: false,
    formStoreKey: 'SUPPLY_CHAIN_PURCHASE_ORDER'
  },
  tableRef
)

const { loaded: clsLoaded, rawMatClsKV } = useMatClsList(toQuery)
const { maxHeight } = useMaxHeight({ paginate: true })
const { handleEnabledChange } = useCrudEnabledChange(
  { CRUD, crud, editEnabled: editPurchaseStatus },
  { enabledField: 'purchaseStatus', enumObj: purchaseStatusEnum, t: 'UNFINISHED', f: 'FINISHED' }
)

const tableLoading = computed(() => !clsLoaded.value || crud.loading)

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.content.map((v) => {
    const basicClassArr = EO.getBits(matClsEnum.ENUM, v.basicClass, 'L')
    v.typeText = baseMaterialTypeEnum.VL[v.purchaseType] + ' - ' + basicClassArr.join(' | ')
    v.branchCompanyId = v.branchCompany ? v.branchCompany.id : undefined
    v.requisitionsSNStr = v.requisitionsSN ? v.requisitionsSN.join('　、　') : ''
    v.projectIds = v.projects ? v.projects.map((p) => p.id) : []
    v.boolPartyA = v.supplyType === orderSupplyTypeEnum.PARTY_A.V
    v.supplierId = v.supplier ? v.supplier.id : undefined
    if (v.auxMaterialIds) {
      v.auxMaterialNames = v.auxMaterialIds.map((id) => {
        const material = rawMatClsKV.value[id]
        if (material) {
          return material.name
        }
        return '-'
      })
    }

    return v
  })
}

function beforeToDetail(row) {
  crud.props.detailType = row.purchaseType === baseMaterialTypeEnum.RAW_MATERIAL.V ? 'rawMaterial' : 'manufactures'
}

// 修改
function beforeToEdit(row) {
  crud.props.formType = row.purchaseType === baseMaterialTypeEnum.RAW_MATERIAL.V ? 'rawMaterial' : 'manufactures'
}

function toQuery() {
  if (clsLoaded.value) {
    crud.queryable = true
    crud.toQuery()
  }
}
</script>