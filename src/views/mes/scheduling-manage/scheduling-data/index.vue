<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <div>
      <div class="table-header">
        <el-tag style="margin-bottom: 8px" size="medium">{{
          crud.query.type === timeTypeEnum.ALL_YEAR.V ? parseTime(crud.query.dateTime, '{y}') : parseTime(crud.query.dateTime, '{y}-{m}')
        }}</el-tag>
      </div>
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%"
        class="collection-table"
        :show-empty-symbol="false"
        :stripe="false"
        return-source-data
        show-summary
        :summary-method="getSummaries"
      >
        <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
        <el-table-column
          v-if="columns.visible('projectName')"
          key="projectName"
          prop="projectName"
          label="所属项目"
          min-width="160"
          :show-overflow-tooltip="true"
        />
        <el-table-column
          v-if="columns.visible('monomerName')"
          key="monomerName"
          prop="monomerName"
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
        >
          <template #default="{ row }">
            <span @click.stop="openDetail">{{ (row.schedulingTotalNetWeight / 1000).toFixed(2) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('completeTotalNetWeight') && crud.query.type === timeTypeEnum.CURRENT_MONTH.V"
          key="completeTotalNetWeight"
          prop="completeTotalNetWeight"
          label="实际完成"
          align="center"
          :show-overflow-tooltip="true"
        >
          <template #default="{ row }">
            <span>{{ (row.completeTotalNetWeight / 1000).toFixed(2) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('completeRate') && crud.query.type === timeTypeEnum.CURRENT_MONTH.V"
          key="completeRate"
          prop="completeRate"
          label="完成率"
          align="center"
          :show-overflow-tooltip="true"
        >
          <template #default="{ row }">
            <span>{{ ((row.completeTotalNetWeight / row.schedulingTotalNetWeight) * 100).toFixed(2) }}%</span>
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
                <div v-if="row.mete?.findIndex((v) => v.date == item) > -1">
                  <template v-for="m in row.mete" :key="m">
                    <template v-if="m.date == item">
                      <span>{{ (m.totalNetWeight / 1000).toFixed(2) }}</span>
                    </template>
                  </template>
                </div>
                <div v-else>0</div>
              </template>
            </el-table-column>
          </template>
        </el-table-column>
        <template v-for="(week, index) in weekList" :key="week">
          <el-table-column
            :label="`第${index + 1}周\n${week.date}`"
            align="center"
            :show-overflow-tooltip="true"
            width="180px"
            v-if="crud.query.type === timeTypeEnum.CURRENT_MONTH.V"
          >
            <template #default="{ row }">
              <div v-if="row.mete?.findIndex((v) => v.date == week.date) > -1">
                <template v-for="w in row.mete" :key="w">
                  <template v-if="w.date == week.date">
                    <span>{{ (w.totalNetWeight / 1000).toFixed(2) }}</span>
                  </template>
                </template>
              </div>
              <div v-else>-</div>
            </template>
          </el-table-column>
        </template>
      </common-table>
      <!-- 排产详情 -->
      <scheduling-detail v-model:visible="drawerVisible" :workshopId="crud.query.workshopId" :dateTime="crud.query.dateTime" />
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/scheduling-data.js'
import { ref, provide } from 'vue'
import { timeTypeEnum } from '@enum-ms/contract'
import { parseTime } from '@/utils/date'
import { mesScheduleDetailPM as permission } from '@/page-permission/mes'
// import { convertUnits } from '@/utils/convert/unit'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import schedulingDetail from './module/scheduling-detail.vue'

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
const weekList = ref([])
const drawerVisible = ref(false)

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
  extraBox: ['.head-container', '.table-header'],
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data?.map((v) => {
    v.projectName = v.project && v.project.shortName ? v.project.serialNumber + ' ' + v.project.shortName : '-'
    v.monomerName = v.monomer && v.monomer.name ? v.monomer.name : '-'
    if (crud.query.type === timeTypeEnum.ALL_YEAR.V) {
      if (v.mete && v.mete.length > 0) {
        v.mete.map((k) => {
          v[Number(k.date)] = k.totalNetWeight ? (k.totalNetWeight / 1000).toFixed(2) : 0
        })
      }
    } else {
      if (v.mete && v.mete.length > 0) {
        v.mete.map((k) => {
          v[k.date] = k
        })
        weekList.value = [...v.mete]
      }
    }

    return v
  })
}

function openDetail() {
  drawerVisible.value = true
}

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 1) {
      sums[index] = '合计'
      return
    }
    if (crud.query.type === timeTypeEnum.ALL_YEAR.V) {
      if (Number(column.label > 0)) {
        const values = data.map((item) => (item.project ? item[column.label] : 0))
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + Number(curr)
          } else {
            return prev
          }
        }, 0)
        sums[index] = Number(sums[index]).toFixed(2)
      }
    }
    if (crud.query.type === timeTypeEnum.CURRENT_MONTH.V) {
      if (index === 3 || index === 4) {
        const values = data.map((item) => item[column.property] || 0)
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + Number(curr)
          } else {
            return prev
          }
        }, 0)
        sums[index] = (Number(sums[index]) / 1000).toFixed(2)
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
  ::v-deep(td.el-table_1_column_19.is-center.is-leaf.el-table__cell .cell) {
    color: #409eff;
    cursor: pointer;
  }
}
</style>
