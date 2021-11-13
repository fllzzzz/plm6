<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <el-table
      ref="table"
      v-loading="crud.loading"
      :data="crud.data"
      style="width: 100%;"
      :max-height="$_tableMaxHeight()"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60">
        <template v-slot="scope">
          <div v-if="scope.row.supplyType == orderSupplyTypeEnum.PARTY_A.V " class="party-tip">甲供</div>
          <span>{{ scope.$index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('organizeTime')" key="organizeTime" :show-overflow-tooltip="true" prop="organizeTime" label="编制日期" align="center" width="100">
        <template v-slot="scope">
          <table-cell-tag v-if="scope.row.settlementStatus == settlementStatusEnum.SETTLED.V" :name="settlementStatusEnum.SETTLED.L" :color="settlementStatusEnum.SETTLED.COLOR" />
          <span>parseTime(scope.row.organizeTime, '{y}-{m}-{d}')</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('orderNo')" key="orderNo" :show-overflow-tooltip="true" prop="orderNo" label="订单号" min-width="160px" />
      <el-table-column v-if="columns.visible('purchaseOrders')" key="purchaseOrders" :show-overflow-tooltip="true" prop="purchaseOrders" label="申购单号" align="center" min-width="120px">
        <template v-slot="scope">
          <el-tooltip
            effect="light"
            :content="`${scope.row.purchaseOrders || '暂无申购单号'}`"
            placement="right"
          >
            <el-tag>查看申购单号</el-tag>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('basicClass')" key="basicClass" :show-overflow-tooltip="true" prop="basicClass" label="物料种类">
        <template v-slot="scope">
          <span v-if="scope.row.basicClass">
            {{ basicClassEnum.VL[scope.row.basicClass] }}
          </span>
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" label="所属项目" min-width="250px">
        <template v-slot="scope">
          <el-tooltip
            effect="light"
            placement="top"
          >
            <template #content>
              <div v-for="item in scope.row.project" :key="item.id" class="project-name" style="margin-bottom:5px;">{{ projectNameFormatter(item,undefined,false) }}</div>
            </template>
            <div style="overflow: hidden;text-overflow: ellipsis;white-space: nowrap;">
              <span v-for="item in scope.row.project" :key="item.id">
                【{{ item.shortName }}】
              </span>
            </div>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('supplierName')" key="supplierName" :show-overflow-tooltip="true" prop="supplierName" label="供应商" min-width="120">
        <template v-slot="scope">
          <span>{{ scope.row.supplierName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalMete')" key="totalMete" :show-overflow-tooltip="true" prop="totalMete" label="合同量" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.totalMete? scope.row.totalMete +'('+ scope.row.meteUnit+')':'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('contractAmount')" key="contractAmount" :show-overflow-tooltip="true" prop="contractAmount" label="合同额（元）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.contractAmount }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceType')" key="invoiceType" :show-overflow-tooltip="true" prop="invoiceType" label="票据类型" align="center" min-width="130">
        <template v-slot="scope">
          <span v-if="scope.row.invoiceType">{{ invoiceTypeEnum.VL[scope.row.invoiceType] }}</span>
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('operator')" key="operator" :show-overflow-tooltip="true" prop="operator" label="操作人" align="center" min-width="100">
        <template v-slot="scope">
          <span>{{ scope.row.operator }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('purchaseStatus')" key="purchaseStatus" prop="purchaseStatus" align="center" min-width="110px">
        <template #header>
          <el-tooltip
            class="item"
            effect="light"
            :content="`采购执行完毕后，该采购单不可在入库办理处选择。\n
            已结算的采购单不可再打开采购状态。`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>采购状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <el-switch
            v-if="checkPermission(permission.editStatus)"
            v-model="scope.row.purchaseStatus"
            :disabled="scope.row.enabledLoading || (scope.row.status === settlementStatusEnum.SETTLED.V && scope.row.purchaseStatus === purchaseStatusEnum.FINISHED.V)"
            active-color="#13ce66"
            :active-value="purchaseStatusEnum.UNFINISHED.V"
            :inactive-value="purchaseStatusEnum.FINISHED.V"
            @change="handleEnabledChange(scope.row)"
          />
          <el-tag v-else :type="scope.row.statusType">{{ purchaseStatusEnum.VL[scope.row.purchaseStatus] }}</el-tag>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-permission="[...permission.edit, ...permission.del, ...permission.download]"
        label="操作"
        width="210px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <el-button size="mini" icon="el-icon-view" type="info" @click="openDetail(scope.row)" />
          <e-operation :data="scope.row.id" :permission="permission.download" />
          <udOperation
            :disabled-edit="scope.row.purchaseStatus == purchaseStatusEnum.FINISHED.V"
            :data="scope.row"
            show-detail
          />
        </template>
      </el-table-column>
    </el-table>
    <!--分页组件-->
    <pagination />
    <!-- 表单 -->
    <m-form />
    <m-detail />
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/wms/order-manage/purchase'
import { ref } from 'vue'
import { invoiceTypeEnum, settlementStatusEnum } from '@enum-ms/finance'
import { orderSupplyTypeEnum, purchaseStatusEnum } from '@enum-ms/wms'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import pagination from '@crud/Pagination'
import eOperation from '@crud/E.operation'
import udOperation from '@crud/UD.operation.vue'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

// crud交由presenter持有
const permission = {
  get: ['wms_purchaseOrder:get'],
  add: ['wms_purchaseOrder:add'],
  edit: ['wms_purchaseOrder:edit'],
  editStatus: ['wms_purchaseOrder:editStatus'],
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

const { CRUD, crud, columns } = useCRUD({
  title: '物料采购订单',
  sort: ['id.desc'],
  permission: { ...permission },
  optShow: { ...optShow },
  crudApi: { ...crudApi }
}, tableRef)

const { handleEnabledChange } = useCrudEnabledChange({ CRUD, crud, editStatus }, { enabledField: 'purchaseStatus', enumObj: purchaseStatusEnum, t: 'UNFINISHED', f: 'FINISHED' })

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.content = data.content.map(v => {
    v.purchaseOrders = v.purchaseOrderNoList && v.purchaseOrderNoList.join(`\n\n`)
    v.orginStauts = v.purchaseStatus
    v.statusType = v.purchaseStatus === purchaseStatusEnum.UNFINISHED.V ? 'warning' : 'success'
    return v
  })
}

// async function handleEnabledChange(data, val) {
//   if (!checkPermission(this.permission.editStatus)) {
//     return
//   }
//   try {
//     await this.$confirm(`此操作将把 “${data.orderNo}” 状态更改为 “${purchaseStatusEnum.VL[val]}”, 是否继续？`, '提示', {
//       confirmButtonText: '确定',
//       cancelButtonText: '取消',
//       type: 'warning'
//     })
//     await crudApi.editStatus({ id: data.id, purchaseStatus: val })
//     this.crud.notify(`“${data.orderNo}” 变更为 “${purchaseStatusEnum.VL[val]}” 成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
//     this.crud.refresh()
//   } catch (error) {
//     console.log(error)
//     data.purchaseStatus = data.orginStauts
//   }
// }

</script>
