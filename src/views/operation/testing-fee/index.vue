<template>
  <div class="app-container">
    <div class="head-container">
      <div style="float: left">
        <el-date-picker
          v-model="year"
          type="year"
          size="small"
          style="width: 100px"
          placeholder="选择年"
          class="filter-item"
          format="YYYY"
          value-format="YYYY"
          :clearable="false"
          :disabled-date="disabledDate"
          @change="fetchTestingFee"
        />
        <project-cascader v-model="projectId" class="filter-item" @change="fetchTestingFee" clearable />
      </div>
      <div style="float: right">
        <export-button
          v-permission="permission.download"
          :fn="downloadTestingFee"
          :params="{ projectId: projectId, year: year }"
        > 
        检测费清单 
        </export-button>
      </div>
    </div>
    <common-table
      ref="tableRef"
      :data="testingFeeList"
      :empty-text="checkPermission(permission.get) ? '暂无数据' : '暂无权限'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column type="index" label="序号" prop="indx" align="center" />
      <el-table-column key="project" prop="project" :show-overflow-tooltip="true" label="项目" min-width="120">
        <template v-slot="scope">
          <span>{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column label="产量（吨）" prop="capacity" key="capacity" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.capacity) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-for="item in itemKeyArr" :label="item.name" :prop="item.key" :key="item.key" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row[item.key]) }}</span>
        </template>
      </el-table-column>
      <el-table-column label="合计" prop="totalAmount" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.totalAmount) }}</span>
        </template>
      </el-table-column>
      <el-table-column label="平均检测费（元/吨）" prop="averageTestingFee" key="averageTestingFee" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.averageTestingFee) }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
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
</template>

<script setup>
import { ref } from 'vue'
import { getTestingFee, downloadTestingFee } from '@/api/operation/testing-fee'

import checkPermission from '@/utils/system/check-permission'
import { testingFeeAnalysisPM as permission } from '@/page-permission/operation'
import useMaxHeight from '@compos/use-max-height'
import { parseTime } from '@/utils/date'
import useDict from '@compos/store/use-dict'
import { projectNameFormatter } from '@/utils/project'
import { toThousand } from '@data-type/number'

// import usePagination from '@compos/use-pagination'
import projectCascader from '@comp-base/project-cascader'
import ExportButton from '@comp-common/export-button/index.vue'

const year = ref(parseTime(new Date(), '{y}'))
const projectId = ref()
const testingFeeList = ref([])
const dict = useDict(['testing_fee_type'])
const itemKeyArr = ref([])

const tableRef = ref()

// const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchTestingFee })

fetchTestingFee()

async function fetchTestingFee() {
  if (!checkPermission(permission.get)) {
    return false
  }
  try {
    itemKeyArr.value = []
    // const { content, totalElements } = await getTestingFee({
    const { content } = await getTestingFee({
      year: year.value,
      projectId: projectId.value
      // ...queryPage
    })
    content.map(v => {
      v.totalAmount = 0
      for (const i in v.checkType) {
        v['fee_' + i] = v.checkType[i]
        v.totalAmount += v.checkType[i]
        if (itemKeyArr.value.findIndex(val => val.key === ('fee_' + i)) < 0) {
          itemKeyArr.value.push({
            key: 'fee_' + i,
            name: dict.value?.testing_fee_type?.find(k => k.id === Number(i)).label
          })
        }
      }
      v.averageTestingFee = v.capacity ? v.totalAmount / v.capacity : v.totalAmount
      return v
    })
    testingFeeList.value = content || []
    // setTotalPage(totalElements)
  } catch (error) {
    console.log('检测费用', error)
  }
}

function disabledDate(time) {
  return time > new Date()
}

//  合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property !== 'project' && column.property !== 'averageTestingFee') {
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
        // sums[index] = sums[index] ? sums[index] : 0
      }
    }
    if (column.property === 'averageTestingFee') {
      sums[index] = sums[2] ? sums[6] / sums[2] : 0
      // console.log('lll', +sums[6], +sums[2])
    }
  })
  const tIndex = [2, 3, 4, 5, 6, 7]
  tIndex.forEach(index => {
    sums[index] = sums[index] ? toThousand(sums[index]) : 0
  })
  return sums
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
</script>
<style lang="scss" scoped>

</style>
