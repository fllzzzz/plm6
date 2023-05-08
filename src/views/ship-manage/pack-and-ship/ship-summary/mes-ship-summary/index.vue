<template>
  <div class="app-container">
    <!--表格渲染-->
    <div style="display: flex">
      <div style="width: 40%; padding-right: 10px">
        <div class="head-container">
          <mHeader />
        </div>
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :data-format="dataFormat"
          :empty-text="crud.emptyText"
          @current-change="handleCurrentChange"
          highlight-current-row
          :max-height="maxHeight"
          style="width: 100%;"
          :stripe="false"
          :showEmptySymbol="false"
        >
          <el-table-column prop="index" label="序号" align="center" width="45" type="index" />
          <el-table-column key="project" prop="project" label="项目" align="left" show-overflow-tooltip min-width="120" />
          <el-table-column key="rate" prop="rate" label="发运数据(吨)" align="center" min-width="160" show-overflow-tooltip>
            <template v-slot="scope">
              <div style="position: relative">
                <el-progress :stroke-width="16" :percentage="scope.row.rate" />
                <span style="position: absolute; top: -2px; left: 0; width: 100%; text-align: right; padding-right: 60px">{{
                  crud.query.weightStatus === weightTypeEnum.NET.V
                    ? (scope.row.sendMete / 1000).toFixed(2) + ' | ' + (scope.row.mete / 1000).toFixed(2)
                    : (scope.row.sendGrossMete / 1000).toFixed(2) + ' | ' + (scope.row.grossMete / 1000).toFixed(2)
                }}</span>
              </div>
            </template>
          </el-table-column>
        </common-table>
        <pagination />
      </div>
      <div style="border-right: 1px solid #ededed; height: calc(100vh - 130px)"></div>
      <div style="width: 59%; padding-left: 10px">
        <projectShipmentDetail
          :workshopId="crud.query.workshopId"
          :weightStatus="crud.query.weightStatus"
          :production-line-type-enum="crud.query.productionLineTypeEnum"
          :currentRow="currentRow"
          v-if="isNotBlank(currentRow)"
          :permission="permission"
        />
        <div class="my-code" v-else>*点击左表操作查看明细</div>
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/ship-manage/pack-and-ship/ship-summary'
import { ref } from 'vue'

import { mesShipSummaryPM as permission } from '@/page-permission/ship-manage'
import { isNotBlank } from '@data-type/index'
import { weightTypeEnum } from '@enum-ms/common'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import projectShipmentDetail from './module/project-shipment-detail.vue'
import pagination from '@crud/Pagination'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([['project', 'parse-project']])

const tableRef = ref()
const currentRow = ref({})

const { crud, CRUD } = useCRUD(
  {
    title: '发运管理',
    sort: [],
    permission: { ...permission },
    dataPath: '',
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ extraBox: ['.head-container'], wrapperBox: ['.app-container'], paginate: false })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.forEach((v) => {
    v.projectId = v.project?.id
    v.rate = v.sendMete && v.mete ? ((v.sendMete / v.mete) * 100).toFixed(2) : 0
  })
}

function handleCurrentChange(val) {
  currentRow.value = val
}
</script>

<style lang="scss" scoped>
::v-deep(.el-progress__text) {
  font-size: 12px !important;
}
</style>
