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
        :stripe="false"
        :show-empty-symbol="false"
        :row-style="handleRowStyle"
      >
        <el-table-column prop="index" label="序号" align="center" width="50" type="index">
          <template #default="{ row, $index }">
            <span>{{ row.type === 1 ? $index + 1 : '' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('projectName')"
          key="projectName"
          prop="projectName"
          label="所属项目"
          min-width="160"
          :show-overflow-tooltip="true"
        >
          <template #default="{ row }">
            <span :class="row.type === 2 && row.projectName !== '合计' ? 'tc-success' : ''">{{ row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('monomerName')"
          key="monomerName"
          prop="monomerName"
          label="单体"
          align="center"
          :show-overflow-tooltip="true"
        >
          <template #default="{ row }">
            <span :class="row.type === 2 && row.projectName !== '合计' ? 'tc-success' : ''">{{ row.monomerName }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('schedulingTotalNetWeight') && crud.query.type === timeTypeEnum.CURRENT_MONTH.V"
          key="schedulingTotalNetWeight"
          prop="schedulingTotalNetWeight"
          label="排产量"
          align="center"
          :show-overflow-tooltip="true"
        >
          <template #default="{ row }">
            <span style="color: #409eff; cursor: pointer" v-if="row.projectName === '合计'" @click.stop="openDetail">{{
              (row.schedulingTotalNetWeight / 1000).toFixed(2) || 0
            }}</span>
            <span v-else>{{ (row.schedulingTotalNetWeight / 1000).toFixed(2) || 0 }}</span>
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
            <span>{{ (row.completeTotalNetWeight / 1000).toFixed(2) || 0 }}</span>
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
            <span>{{ row.completeRate || 0 }}%</span>
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
                      <span :class="row.type === 2 && row.projectName !== '合计' ? 'tc-success' : ''">{{ m.totalNetWeight || 0 }}</span>
                    </template>
                  </template>
                </div>
                <div v-else :class="row.type === 2 && row.projectName !== '合计' ? 'tc-success' : ''">0</div>
              </template>
            </el-table-column>
          </template>
        </el-table-column>
        <template v-if="crud.data.length > 0">
          <template v-for="(week, index) in weekList" :key="week">
            <el-table-column
              :label="`第${index + 1}周\n${week?.date}`"
              align="center"
              prop="monthTotal"
              :show-overflow-tooltip="true"
              width="180px"
              v-if="crud.query.type === timeTypeEnum.CURRENT_MONTH.V"
            >
              <template #default="{ row }">
                <div v-if="row.mete?.findIndex((v) => v.date == week.date) > -1">
                  <template v-for="w in row.mete" :key="w">
                    <template v-if="w.date == week.date">
                      <span>{{ w.totalNetWeight || 0 }}</span>
                    </template>
                  </template>
                </div>
                <div v-else>-</div>
              </template>
            </el-table-column>
          </template>
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
import { schedulingDataPM as permission } from '@/page-permission/mes'
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
  weekList.value = []
  let yearTotal = 0
  let actualYearTotal = 0
  let meteArr = []
  let actualMeteArr = []
  let actualRateArr = []
  let actualRateTotal = 0
  let currentMonthTotal = 0
  const monthData = []
  const currentMonthData = []
  // const actualMonthData = []
  const schedulingTotal = []
  const completeTotal = []
  let schedulingTotalArr = []
  let completeTotalArr = []
  let currentMonthArr = []
  data.content = data?.map((v) => {
    meteArr = []
    actualMeteArr = []
    actualRateArr = []
    schedulingTotalArr = []
    completeTotalArr = []
    currentMonthArr = []
    v.type = 1
    v.projectName = v.project && v.project.shortName ? v.project.serialNumber + ' ' + v.project.shortName : '-'
    // if (v.projectName === '实际完成') {
    //   v.type = 2
    // }
    v.completeRate = v && v.schedulingTotalNetWeight ? ((v.completeTotalNetWeight / v.schedulingTotalNetWeight) * 100).toFixed(2) : 0
    v.monomerName = v.monomer && v.monomer.name ? v.monomer.name : '-'
    if (crud.query.type === timeTypeEnum.ALL_YEAR.V) {
      if (v.mete && v.mete.length > 0) {
        v.mete.map((k) => {
          v[Number(k.date)] = k.totalNetWeight ? (k.totalNetWeight / 1000).toFixed(2) : 0
          k.totalNetWeight = (k.totalNetWeight / 1000).toFixed(2)
          k.completeNetWeight = (k.completeNetWeight / 1000).toFixed(2)
        })
      }
    } else {
      if (v.mete && v.mete.length > 0) {
        v.mete.map((k) => {
          v[k.date] = k
          k.totalNetWeight = (k.totalNetWeight / 1000).toFixed(2) || 0
        })
        weekList.value = v.mete
      }
    }
    for (let m = 1; m <= monthArr.value.length; m++) {
      if (v.mete?.findIndex((o) => Number(o.date) === m) > -1) {
        monthData.push(v.mete[v.mete?.findIndex((o) => Number(o.date) === m)])
      }
      const monthList = monthData.filter((n) => Number(n?.date) === m)
      const totalArr = monthList.map((k) => k.totalNetWeight)
      const actualTotalArr = monthList.map((k) => k.completeNetWeight)
      yearTotal = totalArr.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      actualYearTotal = actualTotalArr.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      actualRateTotal = yearTotal && actualYearTotal ? Number(((actualYearTotal / yearTotal) * 100).toFixed(2)) + '%' : 0 + '%'
      meteArr.push({
        date: monthArr.value[m - 1].toString(),
        totalNetWeight: yearTotal.toFixed(2)
      })
      actualMeteArr.push({
        date: monthArr.value[m - 1].toString(),
        totalNetWeight: actualYearTotal.toFixed(2)
      })
      actualRateArr.push({
        date: monthArr.value[m - 1].toString(),
        totalNetWeight: actualRateTotal
      })
    }

    if (crud.query.type === timeTypeEnum.CURRENT_MONTH.V) {
      schedulingTotal.push(v.schedulingTotalNetWeight)
      completeTotal.push(v.completeTotalNetWeight)
      schedulingTotalArr = schedulingTotal.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      completeTotalArr = completeTotal.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      weekList.value.forEach((p) => {
        console.log(v.mete, p.date)
        if (v.mete.findIndex((e) => e.date === p.date) > -1) {
          currentMonthData.push(v.mete[v.mete.findIndex((e) => e.date === p.date)])
          const currentMonthList = currentMonthData.filter((n) => n.date === p.date)
          const currentTotalArr = currentMonthList.map((k) => k.totalNetWeight)
          currentMonthTotal = currentTotalArr.reduce((pre, cur) => {
            if (cur) {
              return pre + Number(cur)
            } else {
              return pre
            }
          }, 0)
          currentMonthArr.push({
            date: p.date,
            totalNetWeight: currentMonthTotal.toFixed(2)
          })
        }
      })
    }

    return v
  })
  if (data.content.length > 0 && crud.query.type === timeTypeEnum.ALL_YEAR.V) {
    data.content.push(
      {
        type: 2,
        index: '',
        projectName: '合计',
        monomerName: '',
        mete: meteArr
      },
      {
        type: 2,
        index: '',
        projectName: '实际完成',
        monomerName: '',
        mete: actualMeteArr
      },
      {
        type: 2,
        index: '',
        projectName: '完成率',
        monomerName: '',
        mete: actualRateArr
      }
    )
  }
  if (data.content.length > 0 && crud.query.type === timeTypeEnum.CURRENT_MONTH.V) {
    data.content.push({
      type: 2,
      index: '',
      projectName: '合计',
      monomerName: '',
      schedulingTotalNetWeight: schedulingTotalArr,
      completeTotalNetWeight: completeTotalArr,
      completeRate: completeTotalArr && schedulingTotalArr ? Number((completeTotalArr / schedulingTotalArr) * 100).toFixed(2) : 0,
      mete: currentMonthArr
    })
  }
}

function openDetail() {
  drawerVisible.value = true
}

function handleRowStyle({ row, rowIndex }) {
  if (row.projectName === '完成率') {
    return {
      position: 'sticky',
      bottom: '0 !important'
    }
  }
  if (row.projectName === '合计') {
    if (crud.query.type === timeTypeEnum.CURRENT_MONTH.V) {
      return {
        position: 'sticky',
        bottom: '0 !important'
      }
    } else {
      return {
        position: 'sticky',
        bottom: '80px !important'
      }
    }
  }
  if (row.projectName === '实际完成') {
    return {
      position: 'sticky',
      bottom: '40px !important'
    }
  }
}

</script>

<style lang="scss" scoped>
::v-deep(.success-row) {
  // display: table-row;
  position: sticky;
  bottom: 0 !important;
  width: 100%;
  z-index: 2;
  font-weight: 600;
}
</style>
