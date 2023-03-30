<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      class="collection-table"
      :stripe="false"
      :showEmptySymbol="false"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column
        v-if="columns.visible('project')"
        key="project"
        prop="project"
        label="所属项目"
        min-width="160"
        :show-overflow-tooltip="true"
      >
        <template #default="{ row }">
          <span>{{ row.project?.serialNumber }}-{{ row.project?.shortName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monomer.name')"
        key="monomer.name"
        prop="monomer.name"
        label="单体"
        align="center"
        :show-overflow-tooltip="true"
      />
      <el-table-column
        v-if="columns.visible('schedulingTotalNetWeight') && crud.query.type === timeTypeEnum.CURRENT_MONTH.V"
        key="schedulingTotalNetWeight"
        prop="schedulingTotalNetWeight"
        label="排产量"
        align="center"
        :show-overflow-tooltip="true"
      />
      <el-table-column
        v-if="columns.visible('completeTotalNetWeight') && crud.query.type === timeTypeEnum.CURRENT_MONTH.V"
        key="completeTotalNetWeight"
        prop="completeTotalNetWeight"
        label="实际完成"
        align="center"
        :show-overflow-tooltip="true"
      />
      <el-table-column
        v-if="columns.visible('completeRate') && crud.query.type === timeTypeEnum.CURRENT_MONTH.V"
        key="completeRate"
        prop="completeRate"
        label="完成率"
        align="center"
        :show-overflow-tooltip="true"
      >
        <template #default="{ row }">
          <span>{{ (row.completeTotalNetWeight / row.schedulingTotalNetWeight) * 100 }}%</span>
        </template>
      </el-table-column>
      <el-table-column
        align="center"
        label="排产计划及执行（单位：吨）"
        :show-overflow-tooltip="true"
        v-if="crud.query.type === timeTypeEnum.ALL_YEAR.V"
      >
        <template v-for="item in monthArr" :key="item">
          <el-table-column :label="item.toString()" align="center" :show-overflow-tooltip="true">
            <template #default="{ row }">
              <div v-if="row.mete.findIndex((v) => v.date == item) > -1">
                <template v-for="m in row.mete" :key="m">
                  <template v-if="m.date == item">
                    <span>{{ (m.totalNetWeight / 1000).toFixed(2) }}</span>
                  </template>
                </template>
              </div>
              <div v-else>-</div>
            </template>
          </el-table-column>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/scheduling-data.js'
import { ref, provide } from 'vue'
import { timeTypeEnum } from '@enum-ms/contract'
// import { parseTime } from '@/utils/date'
import { mesScheduleDetailPM as permission } from '@/page-permission/mes'
import { convertUnits } from '@/utils/convert/unit'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i)
}

const tableRef = ref()

const { crud, columns, CRUD } = useCRUD(
  {
    title: '排产数据',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    invisibleColumns: [],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

provide('permission', permission)
const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data?.map((v) => {
    return v
  })
}

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (index !== 1 || index !== 2) {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = convertUnits(sums[index], 'kg', 't', 2)
      }
    }
  })
  return sums
}
</script>

<style lang="scss" scoped>
.collection-table {
  ::v-deep(.el-select .el-input__inner) {
    padding-left: 2px;
    padding-right: 5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding: 0 5px;
  }
  ::v-deep(.el-table .cell) {
    padding-left: 2px;
    padding-right: 2px;
  }
}
</style>
