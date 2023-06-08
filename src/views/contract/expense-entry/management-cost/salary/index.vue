<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column v-if="columns.visible('month')" prop="month" label="月份" align="center" width="100">
        <template #default="{ row }">
          <span>{{ row.month }}月</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('employeeQuantity')"
        align="center"
        key="employeeQuantity"
        prop="employeeQuantity"
        :show-overflow-tooltip="true"
        label="员工人数"
      >
        <template #default="{ row }">
          <span>{{ row.employeeQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalWage')"
        align="center"
        key="totalWage"
        prop="totalWage"
        :show-overflow-tooltip="true"
        label="工资总额"
      >
        <template #default="{ row }">
          <span>{{ row.totalWage }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('averageValue')"
        align="center"
        key="averageValue"
        prop="averageValue"
        :show-overflow-tooltip="true"
        label="平均工资（人/元）"
      />
      <el-table-column
        v-if="columns.visible('attachments')"
        align="center"
        key="attachments"
        prop="attachments"
        :show-overflow-tooltip="true"
        label="附件"
      >
        <template #header>
          <el-tooltip class="item" effect="dark" :content="`双击编号可预览文附件`" placement="top">
            <div style="display: inline-block">
              <span>附件</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <!-- <upload-btn ref="uploadRef" v-model:files="scope.row.files" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.pdf,.jpg,.jpeg,.png'"/> -->
          <template v-if="scope.row.attachments && scope.row.attachments.length > 0">
            <div v-for="item in scope.row.attachments" :key="item.id">
              <div style="cursor: pointer; color: #409eff" @dblclick="attachmentView(item)">{{ item.name }}</div>
            </div>
          </template>
        </template>
      </el-table-column>
      <el-table-column align="center" label="操作" width="200px">
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <m-form :query="crud.query" />
    <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/contract/expense-entry/salary'

import { salaryCostPM as permission } from '@/page-permission/contract'
import { DP } from '@/settings/config'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'

import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const tableRef = ref()
const pdfShow = ref(false)
const currentId = ref()
const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([['averageValue', ['to-fixed', 2]]])

const { crud, CRUD, columns } = useCRUD(
  {
    title: '人员工资',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
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
    if (index === 3) {
      sums[index] = 0
      const employeeQuantityList = data.map((v) => v.employeeQuantity)
      const totalWageList = data.map((v) => v.totalWage)
      const employeeQuantitySum = employeeQuantityList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      const totalWageSum = totalWageList.reduce((pre, cur) => {
        if (cur) {
          return pre + Number(cur)
        } else {
          return pre
        }
      }, 0)
      sums[index] = (totalWageSum / employeeQuantitySum).toFixed(DP.YUAN)
      return
    }
    if (column.property === 'employeeQuantity' || column.property === 'totalWage') {
      const values = data.map((item) => Number(item[column.property]))
      let valuesSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      sums[index] = valuesSum
    }
  })
  return sums
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content?.map((v) => {
    v.averageValue = v.totalWage / v.employeeQuantity
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
