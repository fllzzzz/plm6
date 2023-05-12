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
      :default-expand-all="true"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row: { sourceRow: row } }">
          <div class="flex-rsc mtb-20" style="margin-left:30px;">
            <!-- TODO: 理论每次刷新unfreezePermission调用5次，实际调用几十次 -->
            <material-freeze-record
              :stripe="false"
              class="table-border-none"
              :material="row"
              mode="incoming"
              :records="row.recordList"
              @unfreeze-success="crud.toQuery"
            />
          </div>
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns
        :columns="columns"
        :basic-class="basicClass"
        quantityField="frozenQuantity"
        meteField="frozenMete"
        label-prefix="冻结"
      />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" />
      <warehouse-info-columns :columns="columns" :show-project="showProjectInfo" :show-monomer="showProjectInfo" :show-area="showProjectInfo" />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-freeze/raw-material/record'
import { rawMaterialFreezeListPM as permission } from '@/page-permission/wms'

import { computed, ref } from 'vue'
import { matClsEnum } from '@enum-ms/classification'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import MHeader from './module/header'
import Pagination from '@crud/Pagination'

import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import materialFreezeRecord from '@/views/wms/material-freeze/raw-material/components/material-freeze-record.vue'

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
const columnsDataFormat = ref([...materialColumns])

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

// 是否显示项目相关信息
const showProjectInfo = computed(() => crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V)

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
}

// 过滤记录
// eslint-disable-next-line no-unused-vars
function filterRecord(list) {
  if (crud.query.freezeType) {
    return list.filter((v) => v.freezeType === crud.query.freezeType)
  } else {
    return list
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
::v-deep(.table-border-none) {
  .cell {
    height: 30px;
    line-height: 30px;
  }
  width: 1200px;
  th.el-table__cell {
    background-color: #65bdcf;
    color: white;
  }
  td.el-table__cell {
    background-color: #f5f5f5;
  }
}
</style>
