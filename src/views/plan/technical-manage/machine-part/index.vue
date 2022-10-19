<template>
  <div class="app-container">
    <template v-if="pageShow">
      <!--工具栏-->
      <div style="display:flex;">
        <div style="width:40%;margin-right:10px;">
          <div class="head-container">
          <mHeader :project-id="globalProjectId" />
        </div>
        <!--表格渲染-->
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          return-source-data
          :showEmptySymbol="false"
          :max-height="maxHeight"
          @current-change="handleCurrentChange"
          highlight-current-row
          style="width: 100%"
          @sort-change="crud.handleSortChange"
        >
          <el-table-column prop="index" label="序号" align="center" width="55" type="index" />
          <el-table-column v-if="columns.visible('thick')" key="thick" prop="thick" :show-overflow-tooltip="true" label="厚度(mm)" align="center"/>
          <el-table-column
            v-if="columns.visible('material')"
            key="material"
            prop="material"
            :show-overflow-tooltip="true"
            label="材质"
            align="center"
          />
          <el-table-column
            v-if="columns.visible('quantity')"
            key="quantity"
            prop="quantity"
            label="清单量(件/kg)"
            align="left"
            min-width="80px"
          >
            <template v-slot="scope">
              <span style="color:#409eff;">{{ scope.row.quantity+' / '+scope.row.totalNetWeight}}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('stockMete')"
            key="stockMete"
            prop="stockMete"
            :show-overflow-tooltip="true"
            :label="`库存量(kg)`"
            align="center"
            min-width="95px"
          >
            <template v-slot="scope">
              {{ scope.row.stockMete ? scope.row.stockMete.toFixed(DP.COM_WT__KG) : '-' }}
            </template>
          </el-table-column>
        </common-table>
        </div>
        <div style="border-right: 1px solid #ededed; height: calc(100vh - 120px)"></div>
        <div style="width:59%;padding-left:10px;">
          <partDetail :currentRow="currentRow" v-if="isNotBlank(currentRow)" />
          <div class="my-code" v-else>*点击左表操作查看明细</div>
        </div>
      </div>
    </template>
    <template v-else>
      <span style="color:red;font-size:13px;">当前项目内容没有包含构件,请到合同管理中进行配置</span>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-manage/machine-part'
import { ref, watch } from 'vue'

import { isNotBlank } from '@data-type/index'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import { DP } from '@/settings/config'
import { machinePartPM as permission } from '@/page-permission/plan'

import mHeader from './module/header'
import partDetail from './module/part-detail'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const pageShow = ref(true)
const currentRow = ref({})
const { crud, CRUD, columns } = useCRUD(
  {
    title: '零件清单',
    sort: ['id.asc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.machine-part',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

watch(
  () => globalProject.value,
  (val) => {
    if (globalProject.value.projectContentList?.length > 0) {
      pageShow.value = globalProject.value.projectContentList.findIndex(v => v.no === TechnologyTypeAllEnum.STRUCTURE.V) > -1
    } else {
      pageShow.value = false
    }
  },
  { deep: true, immediate: true }
)

CRUD.HOOK.handleRefresh = (crud, data) => {
  const content = data.data
  data.data.content = content
}

function handleCurrentChange(val) {
  currentRow.value = val
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
</style>
