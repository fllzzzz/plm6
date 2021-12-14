<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      :key="`material_freeze_${crud.query.basicClass}`"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="true"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <div class="flex-rcc">
            <!-- TODO: 理论每次刷新unfreezePermission调用5次，实际调用几十次 -->
            <common-table :key="`material_freeze_record_${Math.random()}`" :data="filterRecord(row.recordList)" :stripe="false" class="table-border-none">
              <el-table-column label="序号" type="index" align="center" width="60" />
              <el-table-column key="type" :show-overflow-tooltip="true" prop="type" label="类型" align="center" width="100">
                <template #default="{ row: record }">
                  <span v-parse-enum="{ e: materialFreezeTypeEnum, v: record.freezeType }" v-suffix="'冻结'" />
                </template>
              </el-table-column>
              <el-table-column
                key="documentType"
                :show-overflow-tooltip="true"
                prop="documentType"
                label="对应单据"
                align="center"
                width="120"
              >
                <template #default="{ row: record }">
                  <span v-if="materialFreezeTypeEnum.V[record.freezeType]">{{ materialFreezeTypeEnum.V[record.freezeType].DOC }}</span>
                </template>
              </el-table-column>
              <el-table-column key="document" :show-overflow-tooltip="true" prop="document" label="单据编号" align="center" min-width="120">
                <template #default="{ row: record }">
                  <clickable-permission-span
                    v-if="record.document && record.document.serialNumber"
                    :permission="openDetailPermission(record.freezeType)"
                    @click="openDocumentDetail(record.freezeType, record.document.id)"
                    :text="record.document.serialNumber"
                  />
                </template>
              </el-table-column>
              <el-table-column
                v-if="columns.visible('project')"
                show-overflow-tooltip
                key="project"
                prop="project"
                label="关联项目"
                min-width="170"
              >
                <template #default="{ row: record }">
                  <span v-parse-project="{ project: record.project }" v-empty-text />
                </template>
              </el-table-column>
              <material-unit-quantity-columns :basic-class="basicClass" label-prefix="冻结" outbound-type-mode />
              <el-table-column
                key="operatorName"
                :show-overflow-tooltip="true"
                prop="operatorName"
                label="冻结人"
                align="center"
                width="90"
              />
              <el-table-column key="frozenTime" :show-overflow-tooltip="true" prop="frozenTime" label="冻结日期" align="center" width="100">
                <template #default="{ row: record }">
                  <span v-parse-time="'{y}-{m}-{d}'">{{ record.frozenTime }}</span>
                </template>
              </el-table-column>
              <el-table-column label="操作" width="80px" align="center">
                <template #default="{ row: record }">
                  <!-- TODO: element bug ? 待修复，此处$index 会等于-1-->
                  <common-button
                    v-if="checkUnFreezePermission(record.freezeType)"
                    type="primary"
                    size="mini"
                    @click="toUnfreeze(row, record)"
                  >
                    解 冻
                  </common-button>
                  <span v-else>无权限</span>
                </template>
              </el-table-column>
            </common-table>
          </div>
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="basicClass" label-prefix="冻结" :show-unit="false" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" />
      <warehouse-info-columns :columns="columns" :show-project="crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V" />
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 解冻 -->
    <unfreeze-form v-model:visible="unfreezeFormVisible" :material="currentMaterial" :record="currentRecord" @success="crud.toQuery" />
    <!-- 调拨详情 -->
    <detail-wrapper ref="transferDetailRef" :api="getTransferDetail">
      <transfer-detail />
    </detail-wrapper>
    <detail-wrapper ref="outboundDetailRef" :api="getOutboundDetail">
      <outbound-detail />
    </detail-wrapper>
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/freeze/raw-mat'
import { detail as getTransferDetail } from '@/api/wms/transfer/raw-mat-application-review'
import { detail as getOutboundDetail } from '@/api/wms/outbound/raw-mat-application-review'
import { computed, ref } from 'vue'
import { matClsEnum } from '@enum-ms/classification'
import { materialFreezeTypeEnum, measureTypeEnum, projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import checkPermission from '@/utils/permission'

import MHeader from './module/header'
import DetailWrapper from '@crud/detail-wrapper.vue'
import Pagination from '@crud/Pagination'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import TransferDetail from '@/views/wms/transfer-application-review/raw-mat/module/detail.vue'
import OutboundDetail from '@/views/wms/outbound-application-review/raw-mat/module/detail.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'
import UnfreezeForm from '../components/unfreeze/index.vue'

// crud交由presenter持有
const permission = {
  get: ['wms_raw_mat_freeze_list:get'],
  requisitionsUnFreeze: ['wms_raw_mat_freeze_list:unfreeze_requisitions'],
  outboundUnFreeze: ['wms_raw_mat_freeze_list:unfreeze_outbound'],
  transferUnFreeze: ['wms_raw_mat_freeze_list:unfreeze_transfer'],
  transferDetail: ['wms_transferApplication_review:detail'],
  outboundDetail: ['wms_outboundApplication_review:detail'],
  requisitionsDetail: ['wms_requisitions:detail']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

// 表格ref
const tableRef = ref()
// header ref
const headerRef = ref()
// 展开keys
const expandRowKeys = ref([])
// 解冻显示
const unfreezeFormVisible = ref(false)
// 当前解冻记录
const currentRecord = ref()
// 当前解冻记录对应的物料
const currentMaterial = ref()
// 调拨详情组件
const transferDetailRef = ref()
// 出库详情组件
const outboundDetailRef = ref()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '钢材物料仓',
    sort: ['id.desc'],
    invisibleColumns: [],
    requiredQuery: ['basicClass'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

// 表格高度
const { maxHeight } = useMaxHeight({ paginate: true })

// 基础类型
const basicClass = computed(() => crud.query.basicClass || matClsEnum.STEEL_PLATE.V)

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
  data.content.forEach((row) => {
    row.recordList.forEach((record) => {
      record.curOutboundUnitType = row.curOutboundUnitType
      record.outboundUnit = row.outboundUnit
      record.outboundUnitPrecision = row.outboundUnitPrecision
      if (record.curOutboundUnitType === measureTypeEnum.MEASURE.V) {
        // 实际在出库中使用的数量
        record.corQuantity = record.quantity // 数量
      } else {
        // 核算量
        record.corQuantity = record.mete
      }
    })
  })
}

// 校验解冻权限
function checkUnFreezePermission(freezeType) {
  const permission = unfreezePermission(freezeType)
  return checkPermission(permission)
}

// 解冻权限
function unfreezePermission(freezeType) {
  switch (freezeType) {
    case materialFreezeTypeEnum.REQUISITIONS.V:
      return permission.requisitionsUnFreeze
    case materialFreezeTypeEnum.OUTBOUND.V:
      return permission.outboundUnFreeze
    case materialFreezeTypeEnum.TRANSFER.V:
      return permission.transferUnFreeze
  }
}

// 查看详情权限
function openDetailPermission(freezeType) {
  switch (freezeType) {
    case materialFreezeTypeEnum.REQUISITIONS.V:
      return permission.requisitionsDetail
    case materialFreezeTypeEnum.OUTBOUND.V:
      return permission.outboundDetail
    case materialFreezeTypeEnum.TRANSFER.V:
      return permission.transferDetail
  }
}

// 过滤记录
function filterRecord(list) {
  if (crud.query.freezeType) {
    return list.filter((v) => v.freezeType === crud.query.freezeType)
  } else {
    return list
  }
}

function openDocumentDetail(freezeType, id) {
  switch (freezeType) {
    case materialFreezeTypeEnum.REQUISITIONS.V:
      break
    case materialFreezeTypeEnum.OUTBOUND.V:
      outboundDetailRef.value.toDetail(id)
      break
    case materialFreezeTypeEnum.TRANSFER.V:
      // 打开调拨详情
      transferDetailRef.value.toDetail(id)
      break
  }
}

// 去解冻
function toUnfreeze(material, record) {
  unfreezeFormVisible.value = true
  currentRecord.value = record
  currentMaterial.value = material
}
</script>

<style lang="scss" scoped>
.el-table {
  ::v-deep(.cell) {
    height: 28px;
    line-height: 28px;
  }
}
.table-border-none {
  ::v-deep(.cell) {
    height: 30px;
    line-height: 30px;
  }
  width: 1200px;
  ::v-deep(th.el-table__cell) {
    background-color: #65bdcf;
    color: white;
  }
  ::v-deep(td.el-table__cell) {
    background-color: #f5f5f5;
  }
}
</style>
