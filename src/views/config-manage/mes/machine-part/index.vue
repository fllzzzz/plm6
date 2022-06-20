<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      class="upload-table"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      return-source-data
      :showEmptySymbol="false"
      :stripe="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('productionLineType')" key="productionLineType" prop="productionLineType" align="center" :show-overflow-tooltip="true" label="生产线">
        <template v-slot="scope">
          <span>{{ scope.row.productionLineType ? artifactProductLineEnum.VL[scope.row.productionLineType] : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" align="center" :show-overflow-tooltip="true" label="代表杆件类型" min-width="150">
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('assembleSpecList')" key="assembleSpecList" prop="assembleSpecList" label="组立规格前缀" align="center" min-width="260">
        <template v-slot="scope">
          <template v-if="scope.row.assembleSpecList && scope.row.assembleSpecList.length > 0">
            <div v-for="(item,i) in scope.row.assembleSpecList" :key="item.id">
              <div :class="i === scope.row.assembleSpecList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
                {{ item.specPrefix }}
              </div>
            </div>
          </template>
          <div v-else class="sandwich-cell-bottom"></div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('assembleSpecList1')" key="assembleSpecList1" prop="assembleSpecList1" label="是否有生成工序" align="center" min-width="260">
        <template v-slot="scope">
          <template v-if="scope.row.assembleSpecList && scope.row.assembleSpecList.length > 0">
            <div v-for="(item,i) in scope.row.assembleSpecList" :key="item.id">
              <div :class="i === scope.row.assembleSpecList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
                {{ item.boolSchedulingEnum ? '√' : '-' }}
              </div>
            </div>
          </template>
          <div v-else class="sandwich-cell-bottom"></div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('sort')"
        key="sort"
        prop="sort"
        :show-overflow-tooltip="true"
        label="排序"
        width="80"
        align="center"
      >
        <template v-slot="scope">
          <span>{{ scope.row.sort }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('boolNestEnum')" align="center" prop="boolNestEnum" label="套料选择">
        <template v-slot="scope">
          <span>{{ scope.row.boolNestEnum ? '√' : '-' }}</span>
        </template>
      </el-table-column> -->
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <ud-operation :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/system-config/machine-part-config'
import { ref } from 'vue'

import { artifactProductLineEnum } from '@enum-ms/mes'
import { machinePartConfigPM as permission } from '@/page-permission/config'

import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '母件类型配置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.machinePartConfig',
  paginate: true,
  extraHeight: 40
})
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
.sandwich-cell-top,
.sandwich-cell-bottom {
  padding: 5px;
  height: 40px;
  line-height: 30px;
  box-sizing: border-box;
  overflow: hidden;
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 2px;
  }
}
.sandwich-cell-top {
  border-bottom: 1px solid #dfe6ec;
}
.upload-table {
  ::v-deep(.cell) {
    padding-left: 0;
    padding-right: 0;
  }
  ::v-deep(thead.is-group th) {
    background: #fff;
  }
}
::v-deep(.el-table--small .el-table__cell) {
  padding: 4px 0;
}
</style>
