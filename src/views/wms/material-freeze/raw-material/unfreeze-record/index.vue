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
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <expand-secondary-info v-if="row.material" :basic-class="row.material.basicClass" :row="row.material">
            <p>
              备注：<span v-empty-text>{{ row.remark }}</span>
            </p>
          </expand-secondary-info>
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" spec-merge />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="basicClass" label-prefix="解冻" outbound-type-mode />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" :show-batch-no="false" />
      <warehouse-info-columns :columns="columns" :show-project="crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V" />
      <el-table-column
        v-if="columns.visible('type')"
        key="type"
        :show-overflow-tooltip="true"
        prop="type"
        label="类型"
        align="center"
        width="100"
      >
        <template #default="{ row: record }">
          <span v-parse-enum="{ e: materialFreezeTypeEnum, v: record.freezeType }" v-suffix="'冻结'" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('documentType')"
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
      <el-table-column
        v-if="columns.visible('document')"
        key="document"
        :show-overflow-tooltip="true"
        prop="document"
        label="单据编号"
        align="center"
        min-width="120"
      >
        <template #default="{ row: record }">
          <!-- 当前页面出库申请单无法查看详情（出库申请单会被清空） -->
          <template v-if="record.document && record.document.serialNumber">
            <span v-if="record.freezeType === materialFreezeTypeEnum.OUTBOUND.V">
              {{ record.document.serialNumber }}
            </span>
            <clickable-permission-span
              v-else
              :permission="openDetailPermission(record.freezeType)"
              @click="openDocumentDetail(record.freezeType, record.document.id)"
              :text="record.document.serialNumber"
            />
          </template>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project')"
        show-overflow-tooltip
        key="project"
        prop="project"
        label="单据项目"
        min-width="150"
      >
        <template #default="{ row: record }">
          <span v-parse-project="{ project: record.project, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('founderName')"
        key="founderName"
        :show-overflow-tooltip="true"
        prop="founderName"
        label="解冻人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="解冻时间"
        align="center"
        width="140"
      >
        <template #default="{ row: record }">
          <span v-parse-time="record.createTime" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 调拨详情 -->
    <detail-wrapper ref="transferDetailRef" :api="getTransferDetail">
      <transfer-detail />
    </detail-wrapper>
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-freeze/raw-material/unfreeze-record'
import { computed, ref } from 'vue'
import { detail as getTransferDetail } from '@/api/wms/material-transfer/raw-material/review'
import { materialFreezeTypeEnum, projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import MHeader from './module/header'
import DetailWrapper from '@crud/detail-wrapper.vue'
import Pagination from '@crud/Pagination'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-custom-field-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-custom-field-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-custom-field-columns/warehouse-info-columns/index.vue'
import TransferDetail from '@/views/wms/material-transfer/raw-material/review/module/detail.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'

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
// 调拨详情组件
const transferDetailRef = ref()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '解冻记录',
    sort: ['id.desc'],
    invisibleColumns: ['documentType', 'project'],
    requiredQuery: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

// 表格高度
const { maxHeight } = useMaxHeight({ paginate: true })

// 基础类型
const basicClass = computed(() => crud.query.basicClass)

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  let materialList = []
  data.content.forEach((row) => materialList.push(row.material))
  await setSpecInfoToList(materialList)
  materialList = await numFmtByBasicClass(materialList, {
    toSmallest: false,
    toNum: false
  })
  data.content.forEach((row, index) => {
    row.material = materialList[index]
  })
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

function openDocumentDetail(freezeType, id) {
  switch (freezeType) {
    case materialFreezeTypeEnum.REQUISITIONS.V:
      break
    case materialFreezeTypeEnum.OUTBOUND.V:
      break
    case materialFreezeTypeEnum.TRANSFER.V:
      // 打开调拨详情
      transferDetailRef.value.toDetail(id)
      break
  }
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
