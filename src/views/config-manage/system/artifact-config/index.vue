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
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('classificationName')" key="classificationName" prop="classificationName" align="center" :show-overflow-tooltip="true" label="构件种类" min-width="150">
        <template v-slot="scope">
          <span>{{ scope.row.classificationName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('specPrefixList')" key="specPrefixList" prop="specPrefixList" label="构件前缀" align="center" min-width="260">
        <template v-slot="scope">
          <template v-if="scope.row.specPrefixList && scope.row.specPrefixList.length > 0">
            <div v-for="(item,i) in scope.row.specPrefixList" :key="item.id">
              <div :class="i === scope.row.specPrefixList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
                {{ item.specPrefix }}
              </div>
            </div>
          </template>
          <div v-else class="sandwich-cell-bottom"></div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('specPrefixList')" key="specPrefixList" prop="specPrefixList" label="是否匹配组立" align="center" min-width="260">
        <template v-slot="scope">
          <template v-if="scope.row.specPrefixList && scope.row.specPrefixList.length > 0">
            <div v-for="(item,i) in scope.row.specPrefixList" :key="item.id">
              <div :class="i === scope.row.specPrefixList.length - 1 ? 'sandwich-cell-bottom' : 'sandwich-cell-top'">
                {{ item.boolUseAssemble ? '√' : '-' }}
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
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/system-config/machine-part-config'
import { ref } from 'vue'
import { artifactConfigPM as permission } from '@/page-permission/config'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
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
    title: '构件类型配置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.artifact-config',
  paginate: true,
  extraHeight: 40
})

// CRUD.HOOK.handleRefresh = (crud, { data }) => {
//   boundAllClassifyIds.value = []
//   data.content.forEach((v) => {
//     v.classifyNames = v.classifyLinks.map((v) => v.classifyName).join('、')
//     v.classifyIds = v.boundFinalClassifyIds
//     boundAllClassifyIds.value = boundAllClassifyIds.value.concat(v.boundFinalClassifyIds)
//   })
// }
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
