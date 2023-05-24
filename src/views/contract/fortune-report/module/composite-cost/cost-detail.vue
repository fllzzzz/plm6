<template>
  <common-drawer
    customClass="cost-detail"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    title="费用明细"
    :wrapper-closable="true"
    size="70%"
  >
    <template #content>
      <div v-loading="tableLoading">
        <el-card class="expense-detail">
          <div>{{ props.detailRow.expenseSubjectName }}</div>
          <div>
            总金额 <span class="blue"> <span v-thousand="props.detailRow.reimburseAmount || 0" /> 元</span>
          </div>
          <div>
            {{ props.detailRow.costAscriptionName }}占比 <span class="blue">{{ props.detailRow.costAscriptionRate || 0 }} %</span>
          </div>
          <div v-if="props.detailRow.showRate">
            综合成本占比 <span class="blue">{{ props.detailRow.costRate || 0 }} %</span>
          </div>
          <div class="print-wrap">
            <print-table
              v-permission="permission.printDetail"
              api-key="expenseReimburseRecord"
              :params="{
                ...params,
              }"
              size="mini"
              type="warning"
            />
          </div>
        </el-card>
        <common-table :data-format="columnsDataFormat" :data="detailList" :max-height="maxHeight">
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
    </template>
  </common-drawer>
</template>

<script setup>
import { getExpenseSubject } from '@/api/contract/fortune-report/fortune-report'
import { ref, watch, inject, computed, defineEmits, defineProps } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailRow: {
    type: Object,
    default: () => {}
  }
})

const permission = inject('permission')

const params = computed(() => {
  return {
    projectId: props.detailRow.projectId,
    costAscriptionEnum: props.detailRow.costAscriptionEnum,
    expenseTypeId: props.detailRow.expenseTypeId,
    expenseSubjectId: props.detailRow.expenseSubjectId
  }
})

const tableLoading = ref(false)
const detailList = ref([])
// 列格式转换
const columnsDataFormat = ref([
  ['reimburseDate', ['parse-time', '{y}-{m}-{d}']],
  ['reimburseAmount', 'to-thousand']
])

const emit = defineEmits(['update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.cost-detail',
    extraBox: ['.el-drawer__header', '.expense-detail'],
    wrapperBox: ['.el-drawer__body'],
    paginate: true
  },
  visible
)

// 获取费用归属详情
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await getExpenseSubject({
      ...queryPage,
      ...params.value
    })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取费用归属详情失败', error)
  } finally {
    detailList.value = _list
    tableLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.expense-detail {
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
      padding-left: 20px;
      border-left: 1px solid #ebeef5;
      .blue {
        color: #0079ff;
        font-weight: bold;
      }
    }
  }
}
</style>
