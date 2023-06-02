<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader ref="headerRef">
        <template #viewLeft>
          <!-- <print-table
            v-permission="permission.print"
            api-key="mesProductionStatisticsReport"
            :params="{
              status: crud.query.status,
              startDate: crud.query.startDate,
              endDate: crud.query.endDate,
              weightStatus: crud.query.weightStatus,
            }"
            size="mini"
            type="warning"
            class="print-table"
          /> -->
        </template>
      </mHeader>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading || !loaded"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :data-format="dataFormat"
      :show-empty-symbol="false"
      row-key="id"
      style="width: 100%"
    >
      <el-table-column type="index" prop="index" label="序号" align="center" width="60px" fixed="left" />
      <el-table-column
        v-if="columns.visible('project')"
        :show-overflow-tooltip="true"
        prop="project"
        label="项目"
        header-align="center"
        width="160px"
        fixed="left"
      >
        <template #default="{ row }">
          <span>{{ row.project ? row.project?.serialNumber + '-' + row.project?.name : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('unit')" prop="unit" label="单位" align="center" fixed="left" />
      <el-table-column
        v-if="columns.visible('rawTotalNetWeight')"
        prop="rawTotalNetWeight"
        label="原材料累计出库"
        align="center"
        width="130px"
        fixed="left"
      >
        <template #default="{ row }">
          <span>{{ crud.query.weightStatus === weightTypeEnum.NET.V ? row.rawTotalNetWeight : row.rawTotalGrossWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('rawNetWeight')"
        prop="rawNetWeight"
        label="原材料本月出库"
        align="center"
        width="130px"
        fixed="left"
      >
        <template #default="{ row }">
          <span>{{ crud.query.weightStatus === weightTypeEnum.NET.V ? row.rawNetWeight : row.rawGrossWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('taskNetWeight')" prop="taskNetWeight" label="任务量" align="center" fixed="left">
        <template #default="{ row }">
          <span>{{ crud.query.weightStatus === weightTypeEnum.NET.V ? row.taskNetWeight : row.taskGrossWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('unit')" prop="unit" label="在制品" align="center">
        <template v-for="item in process" :key="item.id">
          <el-table-column
            v-if="
              item.productType &&
              componentTypeEnum.ARTIFACT.V | item.productType &&
              componentTypeEnum.ASSEMBLE.V &&
              item.productionLineTypeEnum & artifactProductLineEnum.TRADITION.V
            "
            :label="item.name"
            align="center"
            width="110px"
          >
            <template #default="{ row }">
              <!-- <div
                v-if="row.processMap[item.id] && row.processMap[item.id]?.inspectionQuantity === row.processMap[item.id]?.quantity"
                style="color: #13ce66"
              >
                √
              </div> -->
              <div>
                <div v-if="row.processMap[item.id]">
                  <span>{{ row.processMap[item.id]?.unQuantity }}</span>
                  <span> / </span>
                  <span>{{
                    crud.query.weightStatus === weightTypeEnum.NET.V
                      ? row.processMap[item.id]?.unNetWeight
                      : row.processMap[item.id]?.unGrossWeight
                  }}</span>
                </div>
                <span v-else> - </span>
              </div>
            </template>
          </el-table-column>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('finishNetWeight')" prop="finishNetWeight" label="制成品" align="center" fixed="right">
        <template #default="{ row }">
          <span>{{ crud.query.weightStatus === weightTypeEnum.NET.V ? row.finishNetWeight : row.finishGrossWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalNetWeight')"
        prop="totalNetWeight"
        :label="`合计\n（在制品+制成品）`"
        align="center"
        width="140px"
        fixed="right"
      >
        <template #default="{ row }">
          <span>{{ crud.query.weightStatus === weightTypeEnum.NET.V ? row.totalNetWeight : row.totalGrossWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('lossNetWeight')" prop="lossNetWeight" label="损耗量" align="center" fixed="right">
        <template #default="{ row }">
          <span>{{ crud.query.weightStatus === weightTypeEnum.NET.V ? row.lossNetWeight : row.lossGrossWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('lossRate')" prop="lossRate" label="损耗率" align="center" fixed="right">
        <template #default="{ row }">
          <span>{{ row.lossRate }}%</span>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/mes/factory-report/product-statistics.js'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useProcess from '@compos/store/use-process'
import pagination from '@crud/Pagination'
import { componentTypeEnum, artifactProductLineEnum } from '@enum-ms/mes'
import { weightTypeEnum } from '@enum-ms/common'
import { mesProductStatisticsPM as permission } from '@/page-permission/mes'
import mHeader from './module/header.vue'

const dataFormat = ref([
  ['rawTotalNetWeight', ['to-fixed', 2]],
  ['rawTotalGrossWeight', ['to-fixed', 2]],
  ['rawNetWeight', ['to-fixed', 2]],
  ['rawGrossWeight', ['to-fixed', 2]],
  ['taskNetWeight', ['to-fixed', 2]],
  ['taskGrossWeight', ['to-fixed', 2]],
  ['finishNetWeight', ['to-fixed', 2]],
  ['finishGrossWeight', ['to-fixed', 2]],
  ['totalNetWeight', ['to-fixed', 2]],
  ['totalGrossWeight', ['to-fixed', 2]],
  ['lossNetWeight', ['to-fixed', 2]],
  ['lossGrossWeight', ['to-fixed', 2]]
])

const tableRef = ref()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, CRUD, columns } = useCRUD(
  {
    title: '生产统计',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { loaded, process } = useProcess()

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content?.map((v) => {
    v.processMap = {}
    v.processDTOList?.forEach((p) => {
      v.processMap[p.id] = p
    })
    return v
  })
}
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>
<style lang="scss" scoped>
</style>
