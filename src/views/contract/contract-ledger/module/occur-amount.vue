<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="发货记录"
    custom-class="occur-detail"
    :wrapper-closable="false"
    size="80%"
  >
    <template #content>
      <common-table
        ref="tableRef"
        :data="tableData"
        :max-height="maxHeight-60"
        :loading="loading"
        style="width: 100%"
        return-source-data
      >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="名称">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="120">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质">
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column key="measure" prop="measure" :show-overflow-tooltip="true" label="计量单位" width="70">
        <template v-slot="scope">
          <span>{{ scope.row.measure }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量">
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column key="nuclear" prop="nuclear" :show-overflow-tooltip="true" label="核算单位" width="70">
        <template v-slot="scope">
          <span>{{ scope.row.nuclear }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalMete" prop="totalMete" :show-overflow-tooltip="true" label="总量">
        <template v-slot="scope">
          <span>{{ scope.row.totalMete }}</span>
        </template>
      </el-table-column>
      <el-table-column key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="单价">
        <template v-slot="scope">
          <span>{{ scope.row.unitPrice?toThousand(scope.row.unitPrice,decimalPrecision.contract):0 }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalPrice" prop="totalPrice" :show-overflow-tooltip="true" label="总价">
        <template v-slot="scope">
          <span>{{ scope.row.totalPrice?toThousand(scope.row.totalPrice,decimalPrecision.contract):0 }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
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
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, watch, ref } from 'vue'
import { occurLog } from '@/api/contract/contract-ledger'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { toThousand } from '@data-type/number'
import usePagination from '@compos/use-pagination'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  projectId: {
    type: [String, Number],
    default: undefined
  }
})

const { decimalPrecision } = useDecimalPrecision()

const tableData = ref([])
const drawerRef = ref()
const loading = ref(false)

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: getOccurLog })

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      getOccurLog()
    } else {
      tableData.value = []
    }
  },
  { deep: true, immediate: true }
)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.occur-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

async function getOccurLog() {
  loading.value = true
  try {
    const { content, totalElements } = await occurLog({ projectId: props.projectId, ...queryPage })
    tableData.value = content || []
    setTotalPage(totalElements)
  } catch (e) {
    console.log('获取变更金额记录', e)
  } finally {
    loading.value = false
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
