<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      class="upload-table"
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :stripe="false"
      return-source-data
      :showEmptySymbol="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="名称" align="center" min-width="150">
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('classifyNames')"
        key="classifyNames"
        prop="classifyNames"
        :show-overflow-tooltip="true"
        label="零件科目"
        min-width="260"
      >
        <template v-slot="scope">
          <span>{{ scope.row.classifyNames }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('links')" key="links" prop="links" :show-overflow-tooltip="true" label="规格前缀索引" align="center" min-width="260">
        <template v-slot="scope">
          <template v-if="scope.row.links && scope.row.links.length > 0">
            <span v-for="(item) in scope.row.links" :key="item.id">
              {{`【${item.keyword}】`}}
            </span>
          </template>
          <div v-else>-</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('boolSectionSteel')"
        key="boolSectionSteel"
        prop="boolSectionSteel"
        :show-overflow-tooltip="true"
        label="是否型材"
        width="100"
        align="center"
      >
        <template v-slot="scope">
          <span>{{isNotBlank(scope.row.boolSectionSteel)?partClsEnum.VL[scope.row.boolSectionSteel]:'-' }}</span>
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
    <mForm :boundAllClassifyIds="boundAllClassifyIds" />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/bridge/part-config'
import { ref } from 'vue'

import { isNotBlank } from '@data-type/index'
import { partClsEnum } from '@enum-ms/bridge'
import { partConfigPM as permission } from '@/page-permission/config'
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
const { crud, columns, CRUD } = useCRUD(
  {
    title: '桥梁零件特征定义',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.part-config',
  paginate: true,
  extraHeight: 40
})

const boundAllClassifyIds = ref([])

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  boundAllClassifyIds.value = []
  data.content.forEach((v) => {
    v.classifyNames = v.classifyLinks.map((v) => v.name).join('、')
    v.classifyIds = v.classifyLinks.map((v) => v.id)
    v.links = []
    for (let i = 0; i < v['specPrefixList'].length; i++) {
      v.links.push({
        keyword: v['specPrefixList'][i]
      })
    }
    boundAllClassifyIds.value = boundAllClassifyIds.value.concat(v.boundFinalClassifyIds)
  })
}
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
