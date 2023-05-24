<template>
  <el-card v-loading="tableLoading" class="card-detail">
    <div>{{ costAscriptionEnum.VL[props.detailRow.costAscription.costAscriptionEnum] }}</div>
    <div>
      总金额 <span class="blue"> <span v-thousand="props.detailRow.exportTaxRebate || 0" /> 元</span>
    </div>
    <div class="print-wrap">
      <print-table
        v-permission="permission.printDetail"
        api-key="expenseReimburseRecord"
        :params="{
          ...params,
          expenseTypeId: currentRow.expenseTypeId,
          expenseSubjectId: currentRow.expenseSubjectId,
        }"
        :disabled="!currentRow.key"
        size="mini"
        type="warning"
      />
    </div>
  </el-card>
  <div class="wrap" :style="{ height: maxHeight - 92 + 'px' }">
    <div class="wrap-left">
      <el-divider>
        <span class="title">分类</span>
      </el-divider>
      <common-table
        :data="list"
        v-loading="tableLoading"
        highlight-current-row
        row-key="key"
        tree-default-expand-all
        :indent="0"
        :data-format="columnsDataFormat"
        :max-height="maxHeight"
        @current-change="currentChange"
      >
        <el-table-column prop="index" key="index" label="序号" align="center" width="80" />
        <el-table-column key="expenseTypeName" prop="expenseTypeName" label="项目" show-overflow-tooltip align="center" />
        <el-table-column key="reimburseAmount" prop="reimburseAmount" label="金额（元）" show-overflow-tooltip align="center" />
        <el-table-column key="rate" prop="rate" label="占比" show-overflow-tooltip align="center">
          <template #default="{ row }">
            <span>{{ row.rate }} %</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <div class="wrap-right">
      <div v-if="currentRow.key">
        <el-divider>
          <span class="title">可用余额信息</span>
        </el-divider>
        <common-table :data-format="columnsDataFormat" :data="detailList" :max-height="maxHeight - 120">
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column key="reimburseDate" prop="reimburseDate" show-overflow-tooltip label="日期" align="center" />
          <el-table-column key="deptName" prop="deptName" show-overflow-tooltip label="部门" align="center" />
          <el-table-column key="reimburseUserName" prop="reimburseUserName" show-overflow-tooltip label="报销人" align="center" />
          <el-table-column key="expenseSubjectName" prop="expenseSubjectName" show-overflow-tooltip label="报销科目" align="center" />
          <el-table-column key="reimburseAmount" prop="reimburseAmount" show-overflow-tooltip label="报销金额" align="center" />
          <el-table-column key="writtenByName" prop="writtenByName" show-overflow-tooltip label="填报人" align="center" />
        </common-table>
        <!--分页组件-->
        <el-pagination
          :total="total"
          :current-page="queryPage.pageNumber"
          :page-size="queryPage.pageSize"
          style="margin-top: 8px"
          layout="total, prev, pager, next, sizes"
          @size-change="handleSizeChange"
          @current-change="handleCurrentChange"
        />
      </div>
      <div v-else class="hint">点击左侧表格行查看详情</div>
    </div>
  </div>
</template>

<script setup>
import { getExpenseSubjectAll, getExpenseSubject } from '@/api/contract/fortune-report/fortune-report'
import { defineProps, nextTick, inject, ref, computed, watch } from 'vue'

import { toFixed } from '@data-type'
import { costAscriptionEnum } from '@enum-ms/config'

import usePagination from '@compos/use-pagination'

const props = defineProps({
  detailRow: {
    type: Object,
    default: () => {}
  },
  maxHeight: {
    type: Number,
    default: 400
  }
})

const params = computed(() => {
  return {
    projectId: props.detailRow.id,
    costAscriptionEnum: props.detailRow.costAscription.costAscriptionEnum
  }
})

watch(
  () => params.value,
  (data) => {
    if (data.projectId) {
      nextTick(() => {
        fetchList()
      })
    }
  },
  { deep: true, immediate: true }
)

const list = ref([])
const tableLoading = ref(false)
const detailList = ref([])
const detailLoading = ref(false)
const currentRow = ref({})
// 列格式转换
const columnsDataFormat = ref([
  ['reimburseDate', ['parse-time', '{y}-{m}-{d}']],
  ['reimburseAmount', 'to-thousand']
])

const permission = inject('permission')
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

function currentChange(row = {}) {
  currentRow.value = row
  fetchDetail()
}

// 获取费用归属详情
async function fetchDetail() {
  let _list = []
  detailLoading.value = true
  try {
    const { content = [], totalElements } = await getExpenseSubject({
      ...queryPage,
      ...params.value,
      expenseTypeId: currentRow.value.expenseTypeId,
      expenseSubjectId: currentRow.value.expenseSubjectId
    })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取费用归属详情失败', error)
  } finally {
    detailList.value = _list
    detailLoading.value = false
  }
}

// 获取费用归属列表
async function fetchList() {
  let _list = []
  tableLoading.value = true
  currentRow.value = {}
  try {
    const costAscriptionAmount = props.detailRow.costAscription.costAscriptionAmount || 0
    _list = await getExpenseSubjectAll(params.value)
    _list.forEach((row, index) => {
      row.reimburseAmount = 0
      row.index = index + 1
      row.key = Math.random()
      row.children = row.subjectList.map((v, i) => {
        row.reimburseAmount += v.reimburseAmount
        v.key = Math.random()
        v.index = `${row.index}-${i + 1}`
        v.expenseTypeId = row.expenseTypeId
        v.expenseTypeName = v.expenseSubjectName
        v.rate = toFixed((v.reimburseAmount / costAscriptionAmount) * 100, 2)
        return v
      })
      row.rate = toFixed((row.reimburseAmount / costAscriptionAmount) * 100, 2)
    })
  } catch (error) {
    console.log('获取费用归属失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.card-detail {
  margin-bottom: 20px;
  ::v-deep(.el-card__body) {
    display: flex;
    align-items: center;
    > div:first-child {
      color: #706f6f;
      font-weight: bold;
      padding-right: 30px;
    }
    > div:not(:first-child, .print-wrap) {
      flex: 1;
      text-align: center;
      border-left: 1px solid #ebeef5;
      .blue {
        color: #0079ff;
        font-weight: bold;
      }
    }
  }
}
.wrap {
  display: flex;
  .wrap-left {
    width: 500px;
    height: 100%;
    overflow-y: auto;
    padding-right: 20px;
    padding-bottom: 20px;
    margin-right: 20px;
    border-right: 1px solid #c8d2e6;
    .el-form {
      border-top: 1px solid #c8d2e6;
      .el-form-item {
        border: 1px solid #c8d2e6;
        border-top-width: 0;
        margin-bottom: 0;
        ::v-deep(label) {
          text-align: center;
          padding-right: 0;
          border-right: 1px solid #c8d2e6;
          background: #f3f8fd;
        }
        > :last-child {
          padding: 0 10px;
        }
      }
    }
  }
  .wrap-right {
    flex: 1;
    min-width: 300px;
    .hint {
      background: #0079ff1a;
      border-left: 6px solid #0079ff;
      line-height: 40px;
      padding-left: 20px;
    }
  }
}
</style>
