<template>
  <el-card v-loading="tableLoading" class="card-detail">
    <div>累计发货</div>
    <div>
      总金额 <span class="blue"> <span v-thousand="props.detailRow.happenedAmount || 0" /> 元</span>
    </div>
    <div class="print-wrap">
      <print-table v-permission="permission.printDetail" api-key="projectHappenedDetail" :params="params" size="mini" type="warning" />
    </div>
  </el-card>
  <common-table ref="tableRef" v-loading="tableLoading" :data-format="columnsDataFormat" :data="list" :max-height="props.maxHeight - 130">
    <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
    <el-table-column prop="name" key="name" label="名称" align="center" show-overflow-tooltip />
    <el-table-column prop="serialNumber" key="serialNumber" label="编号" align="center" show-overflow-tooltip />
    <el-table-column prop="specification" key="specification" label="规格" align="center" show-overflow-tooltip />
    <el-table-column prop="material" key="material" label="材质" align="center" show-overflow-tooltip />
    <el-table-column prop="nuclear" key="nuclear" label="核算单位" align="center" show-overflow-tooltip />
    <el-table-column prop="totalMete" key="totalMete" label="总量" align="center" show-overflow-tooltip />
    <el-table-column prop="unitPrice" key="unitPrice" label="单价" align="right" show-overflow-tooltip />
    <el-table-column prop="totalPrice" key="totalPrice" label="总价" align="right" show-overflow-tooltip />
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
</template>

<script setup>
import { shipRecord } from '@/api/contract/sales-manage/order-tracking'
import { defineProps, nextTick, inject, ref, computed, watch } from 'vue'

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

const permission = inject('permission')
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const params = computed(() => {
  return {
    projectId: props.detailRow.id
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

const tableLoading = ref(false)
const list = ref([])
const columnsDataFormat = ref([
  ['unitPrice', 'to-thousand'],
  ['totalPrice', 'to-thousand']
])

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const { content = [], totalElements } = await shipRecord({ ...params.value, ...queryPage })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取发运记录失败', error)
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
</style>
