<template>
  <div class="app-container">
    <mHeader :project-id="globalProjectId" v-model:modifying="modifying" v-model:lines="lines">
      <template v-slot:customSearch>
        <el-input
          v-model="crud.query.name"
          size="small"
          placeholder="输入名称搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="crud.toQuery"
        />
      </template>
    </mHeader>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :row-class-name="handleRowClassName"
      :cell-class-name="handelCellClassName"
      style="width: 100%"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" fixed />
      <el-table-column
        v-if="columns.visible('areaName')"
        key="areaName"
        prop="areaName"
        fixed
        sortable="custom"
        :show-overflow-tooltip="true"
        label="区域"
        width="120px"
      />
      <productType-base-info-columns
        :productType="componentTypeEnum.ENCLOSURE.V"
        enclosureShowItem
        :category="mesEnclosureTypeEnum.SANDWICH_BOARD.V"
        :columns="columns"
        :fixed="'left'"
        fixedWidth
      />
      <template v-for="workshop in lines">
        <template v-for="line in workshop.productionLineList">
          <el-table-column
            v-if="line.selected"
            :key="line.id"
            :prop="`productionLine_${line.id}`"
            :label="line.name"
            align="center"
            width="150px"
          >
            <template v-slot:header>
              <span class="ellipsis-text">【{{ workshop.name }}】</span>
              <span class="ellipsis-text">{{ line.name }}</span>
            </template>
            <template v-slot="scope">
              <el-input-number
                v-if="modifying"
                v-model="scope.row.schedulingMap[line.id].quantity"
                :step="1"
                :min="scope.row.schedulingMap[line.id].sourceQuantity"
                :max="scope.row.quantity"
                size="mini"
                controls-position="right"
                style="width: 100%"
                @change="handleQuantityChange(scope.row, line, $event)"
              />
              <span v-else>{{ scope.row.schedulingMap[line.id].quantity }}</span>
            </template>
          </el-table-column>
        </template>
      </template>
      <el-table-column min-width="1px" />
      <el-table-column
        v-if="columns.visible('unassignQuantity')"
        key="unassignQuantity"
        prop="unassignQuantity"
        sortable="custom"
        fixed="right"
        label="未分配"
        align="center"
        width="105px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.sourceUnassignQuantity }}</span>
          <span
v-if="modifying && scope.row.unassignQuantity !== scope.row.sourceUnassignQuantity"
            >▶<span :style="{ color: scope.row.unassignQuantity < scope.row.sourceUnassignQuantity ? '#11b95c' : 'red' }">{{
              scope.row.unassignQuantity
            }}</span></span
          >
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('assignQuantity')"
        key="assignQuantity"
        prop="assignQuantity"
        sortable="custom"
        fixed="right"
        label="已分配"
        align="center"
        width="105px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.sourceAssignQuantity }}</span>
          <span
v-if="modifying && scope.row.assignQuantity !== scope.row.sourceAssignQuantity"
            >▶<span :style="{ color: scope.row.assignQuantity > scope.row.sourceAssignQuantity ? '#11b95c' : 'red' }">{{
              scope.row.assignQuantity
            }}</span></span
          >
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        sortable="custom"
        fixed="right"
        label="数量"
        align="center"
        width="70px"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/scheduling/enclosure'
import { provide, ref } from 'vue'

import { componentTypeEnum, processTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'
// import checkPermission from '@/utils/system/check-permission'
import { DP } from '@/settings/config'
import { mapGetters } from '@/store/lib'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useSchedulingIndex from '@compos/mes/scheduling/use-scheduling-index'
import pagination from '@crud/Pagination'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'
import mHeader from '@/views/mes/scheduling-manage/scheduling/components/scheduling-header'

// crud交由presenter持有
const permission = {
  get: ['artifactScheduling:get'],
  editStatus: ['artifactScheduling:editStatus'],
  save: ['artifactScheduling:save'],
  clear: ['artifactScheduling:clearWithOneClick']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

provide('needTableColumns', [
  { label: '名称', width: '120px', field: 'name' },
  { label: '板型', width: '120px', field: 'plate' },
  { label: `板厚\n(mm)`, width: '80px', field: 'thickness', toFixed: true, DP: DP.MES_ENCLOSURE_T__MM },
  { label: `有效宽度\n(mm)`, width: '80px', field: 'width', toFixed: true, DP: DP.MES_ENCLOSURE_W__MM }
])
provide('productType', componentTypeEnum.ENCLOSURE.V)
provide('processType', processTypeEnum.TWICE.V)

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '夹芯板排产',
    permission: { ...permission },
    sort: [],
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['areaId'],
    invisibleColumns: ['areaName', 'length', 'totalArea', 'totalLength', 'brand', 'type', 'capacity', 'remark'],
    queryOnPresenterCreated: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
const { globalProjectId } = mapGetters(['globalProjectId'])
const { lines, modifying, handleRowClassName, handelCellClassName, handleQuantityChange } = useSchedulingIndex()

CRUD.HOOK.beforeToQuery = () => {
  crud.query.category = mesEnclosureTypeEnum.SANDWICH_BOARD.V
}
</script>

<style lang="scss" scoped>
::v-deep(.el-table) {
  .cell {
    line-height: 30px;
  }
}
// /deep/.abnormal-row {
//   background: linear-gradient(to right, #ffecec 0%, #ffffff 100%);
// }
::v-deep(.el-input__inner) {
  font-size: 14px;
}
</style>
