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
          style="width: 100%"
          :stripe="false"
          :showEmptySymbol="false"
        >
          <el-table-column prop="index" label="序号" align="center" width="45" type="index" />
          <el-table-column key="project" prop="project" label="项目" align="left" show-overflow-tooltip min-width="120" />
          <el-table-column key="rate" prop="rate" label="发运数据(米)" align="center" min-width="160" show-overflow-tooltip>
            <template v-slot="scope">
              <div style="position: relative">
                <el-progress color="#67C23A" :stroke-width="16" :percentage="scope.row.ratio" />
                <span style="position: absolute; top: -2px; left: 0; width: 100%; text-align: right; padding-right: 60px">{{
                  (scope.row.sendTotalLength / 1000).toFixed(3) + ' | ' + (scope.row.totalLength / 1000).toFixed(3)
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
          :category-list="categoryList"
          v-if="isNotBlank(currentRow)"
          :permission="permission"
        />
        <div class="my-code" v-else>*点击左表操作查看明细</div>
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/ship-manage/pack-and-ship/enclosure-ship-summary'
import { ref } from 'vue'
import { deepClone } from '@/utils/data-type'
import { componentTypeEnum } from '@enum-ms/mes'
import { enclosureShipSummaryPM as permission } from '@/page-permission/ship-manage'
import { isNotBlank } from '@data-type/index'
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
const categoryList = ref([])
const categoryData = ref([])

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
    v.ratio = v.ratio?.toFixed(2)
  })
}

function handleCurrentChange(val) {
  currentRow.value = val
  categoryData.value = val?.project?.projectContentList?.filter(v => v.categoryType === componentTypeEnum.ENCLOSURE.V)
  categoryList.value = deepClone(categoryData.value)
  categoryList.value?.push({
    id: 20,
    name: '配套制品',
    no: 64,
    categoryType: 4
  })
}
</script>

<style lang="scss" scoped>
::v-deep(.el-progress__text) {
  font-size: 12px !important;
}
</style>
