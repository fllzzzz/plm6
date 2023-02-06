<template>
  <div class="app-container">
    <div v-show="!detailRow.process?.id" class="my-code" style="width: 100%">*点击左侧表格行查看详情</div>
    <div v-show="detailRow.process?.id" style="width: 100%">
      <div class="production-detail" style="display: flex; justify-content: space-between">
        <div>
          <el-input
            v-model.trim="userName"
            placeholder="可输入姓名搜索"
            class="filter-item"
            style="width: 200px; margin-bottom: 8px"
            size="small"
            clearable
            @keyup.enter="fetchDetail"
          />
          <common-button
            class="filter-item"
            size="mini"
            type="success"
            icon="el-icon-search"
            style="margin-left: 8px"
            @click.stop="searchQuery"
          >
            搜索
          </common-button>
          <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
            重置
          </common-button>
        </div>
        <div>
          <export-button
            class="filter-item"
            :fn="exportListFn"
            v-permission="permission.export"
            :params="{ processId: detailRow.process?.id, userName: userName, ...props.commonParams }"
          >
            工资清单
          </export-button>
        </div>
      </div>
      <common-table
        ref="tableRef"
        :data="workshopList"
        :empty-text="'暂无数据'"
        :max-height="maxHeight"
        row-key="id"
        style="width: 100%"
        :header-cell-style="headerStyle"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left" />
        <el-table-column
          align="center"
          key="workshop.name"
          prop="workshop.name"
          :show-overflow-tooltip="true"
          label="车间"
          min-width="120px"
          fixed="left"
        >
          <template #default="{ row }">
            <span>{{ row.workshop?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column
          align="center"
          key="productionLine.name"
          prop="productionLine.name"
          :show-overflow-tooltip="true"
          label="产线"
          min-width="120px"
          fixed="left"
        >
          <template #default="{ row }">
            <span>{{ row.productionLine?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column
          align="center"
          key="groups.name"
          prop="groups.name"
          :show-overflow-tooltip="true"
          label="生产组"
          min-width="120px"
          fixed="left"
        >
          <template #default="{ row }">
            <span>{{ row.groups?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column
          align="center"
          key="team.name"
          prop="team.name"
          :show-overflow-tooltip="true"
          label="班组"
          min-width="120px"
          fixed="left"
        >
          <template #default="{ row }">
            <span>{{ row.team?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column
          align="center"
          key="totalPrice"
          prop="totalPrice"
          :show-overflow-tooltip="true"
          label="总额"
          min-width="120px"
          fixed="left"
        >
          <template #default="{ row }">
            <span>{{ row.totalPrice?.toFixed(2) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="total" align="center" :key="'_' + item" :show-overflow-tooltip="true" v-for="item in yearList" :label="item">
          <template v-for="val in dayList" :key="val?.split('/')[2]">
            <el-table-column
              v-if="new Date(val).getFullYear() == item"
              prop="sum"
              align="center"
              :show-overflow-tooltip="true"
              :label="parseTime(new Date(val).getTime(), '{m}/{d}')"
              min-width="120"
            >
              <template v-slot="scope">
                <div v-if="scope.row.priceList.findIndex((v) => v.dayTime == val) > -1">
                  <template v-for="day in scope.row.priceList" :key="day">
                    <template v-if="day.dayTime == val">
                      <span>{{ day.price?.toFixed(2) }}</span>
                    </template>
                  </template>
                </div>
                <template v-else>
                  <span>/</span>
                </template>
              </template>
            </el-table-column>
          </template>
        </el-table-column>
      </common-table>
      <!-- 分页 -->
      <!-- <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      /> -->
    </div>
  </div>
</template>

<script setup>
import { ref, defineProps, watch, inject } from 'vue'
import { detail, exportListFn } from '@/api/mes/production-line-wage-statistics/production-statistics'
import { parseTime } from '@/utils/date'
// import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import ExportButton from '@comp-common/export-button/index.vue'

const permission = inject('permission')
const props = defineProps({
  detailRow: {
    type: Object,
    default: () => {}
  },
  commonParams: {
    type: Object
  }
})
const { maxHeight } = useMaxHeight({
  extraBox: ['.production-detail'],
  paginate: true
})
const userName = ref()
const workshopList = ref([])
const dayList = ref([])
const yearList = ref([])
const monthList = ref([])
const monthData = ref([])

// const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

function getDateList(start, end, long) {
  let startData = start
  const count = (end - start) / (24 * 60 * 60 * 1000)
  var dateData = []
  for (var i = 0; i <= count; i++) {
    dateData.push(parseTime(startData, '{y}/{m}/{d}'))
    startData += long
  }
  dayList.value = dateData
}
watch(
  () => props.detailRow.process?.id,
  (val) => {
    if (val) {
      fetchDetail()
    }
  }
)
async function fetchDetail() {
  try {
    const { content } = await detail({
      processId: props.detailRow.process?.id,
      userName: userName.value,
      ...props.commonParams
    })
    // setTotalPage(totalElements)
    content?.forEach((v) => {
      v.startTime = props.commonParams?.startTime
      v.endTime = props.commonParams?.endTime
    })
    getDateList(Number(props.commonParams?.startTime), Number(props.commonParams?.endTime), 24 * 60 * 60 * 1000)
    yearList.value = []
    dayList.value.forEach((v) => {
      if (yearList.value.indexOf(v.split('/')[0]) === -1) {
        yearList.value.push(v.split('/')[0])
      }
      yearList.value.sort(function (a, b) {
        return a - b
      })
    })
    dayList.value.forEach((v) => {
      if (monthList.value.indexOf(v.split('/')[1]) === -1) {
        monthList.value.push(v.split('/')[1])
      }
    })
    workshopList.value = content || []
  } catch (error) {
    console.log('获取对应工序下的详情失败', error)
  }
}

// 搜索
function searchQuery() {
  fetchDetail()
}

// 重置
function resetQuery() {
  userName.value = undefined
  fetchDetail()
}

function headerStyle({ row, column, rowIndex, columnIndex }) {
  monthData.value = []
  monthData.value.push(column.label?.split('/')[0])
  if (column.property === 'sum') {
    if (yearList.value.length === 1 && monthList.value.length === 1) {
      return ''
    } else if (yearList.value.length === 1 && monthList.value.length > 1) {
      if (monthData.value[0] === monthList.value[0]) {
        return 'background: #e1f3d8'
      } else if (monthData.value[0] === monthList.value[1]) {
        return 'background: #faecd8'
      }
    } else if (yearList.value.length > 1 && monthList.value.length > 1) {
      if (monthData.value[0] === monthList.value[0]) {
        return 'background: #e1f3d8'
      } else {
        return 'background: #faecd8'
      }
    }
  }
  if (column.property === 'total') {
    if (yearList.value.length === 1 && (monthList.value.length === 1 || monthList.value.length > 1)) {
      return ''
    } else {
      if (columnIndex >= 6 && (columnIndex - 4) % 2 === 0) {
        return 'background: #d1edc4'
      } else if (columnIndex >= 7 && (columnIndex - 5) % 2 === 0) {
        return 'background: #f8e3c5'
      }
    }
  }
  // if (column.property === 'total' && yearList.value.length > 1 && monthList.value.length > 1) {
  //   if (columnIndex >= 6 && (columnIndex - 4) % 2 === 0) {
  //     return 'background: #d1edc4'
  //   } else if (columnIndex >= 7 && (columnIndex - 5) % 2 === 0) {
  //     return 'background: #f8e3c5'
  //   }
  // }
  // if (column.property === 'sum' && yearList.value.length > 1 && monthList.value.length > 1) {
  //   if (monthData.value[0] === monthList.value[0]) {
  //     return 'background: #e1f3d8'
  //   } else if (monthData.value[0] === monthList.value[1]) {
  //     return 'background: #faecd8'
  //   }
  // }
  // if (yearList.value.length === 1 && column.property === 'sum' && monthList.value.length > 1) {
  //   if (monthData.value[0] === monthList.value[0]) {
  //     return 'background: #e1f3d8'
  //   } else if (monthData.value[0] === monthList.value[1]) {
  //     return 'background: #faecd8'
  //   }
  // }
}
</script>

<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
