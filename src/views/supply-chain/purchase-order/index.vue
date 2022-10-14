<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="tableLoading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="serialNumber"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="serialNumber">
        <template #default="{ row }">
          <p v-if="isNotBlank(row.auxMaterialIds)">
            辅材明细：<span>{{ row.auxMaterialNames }}</span>
          </p>
          <p>
            关联项目：<span>{{ row.projectsFullName }}</span>
          </p>
          <p>
            关联申购单：<span>{{ row.requisitionsSNStr }}</span>
          </p>
          <p>
            备注：<span>{{ row.remark }}</span>
          </p>
        </template>
      </el-expand-table-column>
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
        width="130"
      >
        <template #default="{ row }">
          <table-cell-tag
            v-if="row.settlementStatus == settlementStatusEnum.SETTLED.V"
            :name="settlementStatusEnum.SETTLED.L"
            :color="settlementStatusEnum.SETTLED.COLOR"
          />
          {{ row.createTime }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="采购合同编号"
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
      />
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
      />
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
            :content="`采购执行完毕后，该采购合同不可在入库办理处选择。\n
            已结算的采购合同不可再打开采购状态。`"
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
        v-if="columns.visible('applicantName')"
        key="applicantName"
        :show-overflow-tooltip="true"
        prop="applicantName"
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
      />
      <!--编辑与删除-->
      <el-table-column label="操作" width="180px" align="center" fixed="right">
        <template #default="{ row }">
          <!-- <e-operation :data="row.id" :permission="permission.download" /> -->
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
    <m-raw-material-form />
    <m-raw-material-detail />
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
import { wmsReceiptColumns } from '@/utils/columns-format/wms'
import { isNotBlank } from '@/utils/data-type'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import pagination from '@crud/Pagination'
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
  del: false,
  download: false
}

const tableRef = ref()
const expandRowKeys = ref([])
// 表格列数据格式转换
const columnsDataFormat = ref([
  ...wmsReceiptColumns,
  ['invoiceType', ['parse-enum', invoiceTypeEnum]],
  ['amount', 'to-thousand'],
  ['requisitionsSNStr', 'empty-text'],
  ['remark', 'empty-text'],
  ['auxMaterialNames', 'split']
])
const { CRUD, crud, columns } = useCRUD(
  {
    title: '采购合同',
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

function toQuery() {
  if (clsLoaded.value) {
    crud.queryable = true
    crud.toQuery()
  }
}
</script>
