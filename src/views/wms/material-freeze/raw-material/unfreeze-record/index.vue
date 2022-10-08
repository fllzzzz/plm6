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
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <expand-secondary-info v-if="row.material" :basic-class="row.material.basicClass" :row="row.material">
            <p>
              备注：<span>{{ row.remark }}</span>
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
      <warehouse-info-columns :columns="columns" :show-project="showProjectInfo" :show-monomer="showProjectInfo" :show-area="showProjectInfo" />
      <el-table-column
        v-if="columns.visible('freezeTypeName')"
        key="freezeTypeName"
        :show-overflow-tooltip="true"
        prop="freezeTypeName"
        label="类型"
        align="center"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('receiptType')"
        key="receiptType"
        :show-overflow-tooltip="true"
        prop="receiptType"
        label="对应单据"
        align="center"
        width="120"
      >
        <template #default="{ row: record }">
          <span v-if="materialFreezeTypeEnum.V[record.freezeType]">{{ materialFreezeTypeEnum.V[record.freezeType].DOC }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('receipt')"
        key="receipt"
        :show-overflow-tooltip="true"
        prop="receipt"
        label="单据编号"
        align="center"
        min-width="120"
      >
        <template #default="{ row: record }">
          <!-- 当前页面出库申请单无法查看详情（出库申请单会被清空） -->
          <receipt-sn-clickable :receipt-types="['PREPARATION', 'OUTBOUND_APPLY', 'TRANSFER', 'REJECTED']" :receipt="record.receipt" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project')"
        show-overflow-tooltip
        key="project"
        prop="project"
        label="单据项目"
        min-width="150"
      />
      <el-table-column
        v-if="columns.visible('applicantName')"
        key="applicantName"
        :show-overflow-tooltip="true"
        prop="applicantName"
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
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-freeze/raw-material/unfreeze-record'
import { rawMaterialUnFreezeListPM as permission } from '@/page-permission/wms'

import { computed, ref } from 'vue'
import { materialFreezeTypeEnum, projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialNestedColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import MHeader from './module/header'
import Pagination from '@crud/Pagination'

import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-custom-field-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-custom-field-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-custom-field-columns/warehouse-info-columns/index.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'

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
// 表格列数据格式转换
const columnsDataFormat = ref([
  ['remark', 'empty-text'],
  ['createTime', 'parse-time'],
  ['project', ['parse-project', { onlyShortName: true }]],
  ['projectFullName', 'parse-project', { source: 'project' }],
  ['freezeTypeName', ['parse-enum', materialFreezeTypeEnum], ['suffix', '冻结'], { source: 'freezeType' }],
  ...materialNestedColumns
])
const { CRUD, crud, columns } = useCRUD(
  {
    title: '解冻记录',
    sort: ['id.desc'],
    invisibleColumns: ['receiptType', 'project'],
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

// 显示项目信息
const showProjectInfo = computed(() => crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V)

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
