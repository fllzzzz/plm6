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
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        align="center"
        :show-overflow-tooltip="true"
        label="代表杆件类型"
        min-width="150"
      >
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('classifyNames')"
        key="classifyNames"
        prop="classifyNames"
        :show-overflow-tooltip="true"
        label="型材科目"
        min-width="260"
      >
        <template v-slot="scope">
          <span>{{ scope.row.classifyNames }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('assembleSpecList')"
        key="assembleSpecList"
        prop="assembleSpecList"
        label="部件规格前缀"
        :show-overflow-tooltip="true"
        align="center"
        min-width="260"
      >
        <template v-slot="scope">
          <template v-if="scope.row.assembleSpecList && scope.row.assembleSpecList.length > 0">
            <span v-for="item in scope.row.assembleSpecList" :key="item.id">
              {{ item.specPrefix + '  ' + (scope.row.boolSectionSteel ? '【' + (item.specIndex ? item.specIndex : '全部') + '】' : '') }}
            </span>
          </template>
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('assembleSpecList1')" key="assembleSpecList1" prop="assembleSpecList1" label="是否有生成工序" align="center" min-width="260">
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
      </el-table-column> -->
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
        v-if="checkPermission([...permission.del, ...permission.edit,...permission.audit])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button v-if="scope.row.auditStatus===1 && checkPermission(permission.audit)" type="success" size="small" @click="detailInfo=scope.row;auditVisible=true">审核</common-button>
          <ud-operation v-if="scope.row.auditStatus!==1" :data="scope.row" :permission="permission"/>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm :boundAllClassifyIds="boundAllClassifyIds" />
    <audit-form v-model="auditVisible" :detailInfo="detailInfo" @success="crud.toQuery" />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/system-config/machine-part-config'
import { ref } from 'vue'

import { machinePartConfigPM as permission } from '@/page-permission/config'
// import { matClsEnum } from '@enum-ms/classification'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'
import auditForm from './module/audit-form'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailInfo = ref({})
const auditVisible = ref(false)
const { CRUD, crud, columns } = useCRUD(
  {
    title: '部件特征定义',
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

const boundAllClassifyIds = ref([])

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  boundAllClassifyIds.value = []
  data.content.forEach((v) => {
    if (v.boolSectionSteel) {
      v.classifyNames = v.classifyLinks.map((v) => v.classifyName).join('、')
      v.classifyIds = v.classifyLinks.map((v) => v.classifyId)
      v.newClassifyNames = v.newClassifyLinks?.map((v) => v.classifyName).join('、')
      v.newClassifyIds = v.newClassifyLinks?.map((v) => v.classifyId)
      boundAllClassifyIds.value = boundAllClassifyIds.value.concat(v.classifyIds)
    }
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
