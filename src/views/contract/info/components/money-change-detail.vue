<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    :before-close="handleClose"
    title="合同金额变更记录"
    :wrapper-closable="false"
    size="1200px"
    custom-class="money-change"
  >
    <template #content>
      <common-table
        ref="tableRef"
        :data="list"
        :max-height="maxHeight"
        style="width: 100%"
        :dataFormat="dataFormat"
      >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="createTime" prop="createTime" :show-overflow-tooltip="true" label="变更日期" width="150px" />
      <el-table-column
        key="amount"
        prop="amount"
        :show-overflow-tooltip="true"
        label="变更金额(元)"
        min-width="180"
      >
        <template v-slot="scope">
            <span>{{toThousand(scope.row.contractAmount,decimalPrecision.contract)}}</span>
            <span>{{scope.row.contractAmount<scope.row.changeAmount?'<':'>'}}</span>
            <span :class="scope.row.contractAmount>scope.row.changeAmount?'tip-red':'tip-green'">{{ toThousand(scope.row.changeAmount,decimalPrecision.contract) }}</span>
        </template>
      </el-table-column>
      <el-table-column key="changeContent" prop="changeContent" :show-overflow-tooltip="true" label="变更内容" min-width="120px" />
      <el-table-column key="userName" prop="userName" :show-overflow-tooltip="true" label="变更发起人" />
      <el-table-column key="auditorName" prop="auditorName" :show-overflow-tooltip="true" label="审核人" />
      <el-table-column key="auditStatus" prop="auditStatus" :show-overflow-tooltip="true" label="审核状态">
        <template v-slot="scope">
          <el-tag :type="auditTypeEnum.V[scope.row.sourceRow.auditStatus].T">
            {{scope.row.auditStatus}}
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column key="auditTime" prop="auditTime" :show-overflow-tooltip="true" label="审核日期" width="150px" />
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
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'
import { get } from '@/api/contract/change-audit-log'

import { toThousand } from '@data-type/number'
import { auditTypeEnum, contractChangeTypeEnum } from '@enum-ms/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  detailInfo: {
    type: Array,
    default: () => {}
  },
  modelValue: {
    type: Boolean,
    require: true
  }
})

const drawerRef = ref()

const emit = defineEmits(['success', 'update:modelValue'])

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

watch(visible, (val) => {
  if (val) {
    fetchList()
  }
})

const list = ref([])
const tableLoading = ref(false)

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.money-change',
    extraBox: ['.el-drawer__header', '.header-div'],
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  visible
)

const dataFormat = ref([
  ['createTime', 'parse-time'],
  ['auditStatus', ['parse-enum', auditTypeEnum]],
  ['auditTime', 'parse-time']
])

// 获取变更明细
async function fetchList() {
  let _list = []
  if (!props.detailInfo.id) {
    list.value = _list
    return
  }
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await get({ projectId: props.detailInfo.id, type: contractChangeTypeEnum.CONTRACT_AMOUNT.V, ...queryPage })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('变更明细', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.tip-red{
  color:red;
}
.tip-green{
  color:#67c23a;
}
</style>
