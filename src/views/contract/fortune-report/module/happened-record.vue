<template>
  <common-drawer
    customClass="happened-record-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="`发货记录 ${props.detailRow.project}`"
    :wrapper-closable="true"
    size="100%"
  >
    <template #titleRight>
      <el-tag effect="plain" type="warning" size="medium">发货总额：{{ props.detailRow.happenedAmount }}</el-tag>
      <div class="print-wrap">
        <print-table v-permission="permission.printDetail" api-key="projectHappenedDetail" :params="params" size="mini" type="warning" />
      </div>
    </template>
    <template #content>
      <common-table :data="list" :data-format="columnsDataFormat" :max-height="maxHeight" style="width: 100%;">
        <el-table-column type="index" prop="index" label="序号" align="center" min-width="60" />
        <el-table-column prop="name" key="name" label="名称" align="center" show-overflow-tooltip min-width="80" />
        <el-table-column prop="serialNumber" key="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="80" />
        <el-table-column prop="specification" key="specification" label="规格" align="center" show-overflow-tooltip min-width="80" />
        <el-table-column prop="material" key="material" label="材质" align="center" show-overflow-tooltip min-width="80" />
        <el-table-column prop="nuclear" key="nuclear" label="核算单位" align="center" show-overflow-tooltip min-width="120" />
        <el-table-column prop="totalMete" key="totalMete" label="总量" align="center" show-overflow-tooltip min-width="150" />
        <el-table-column prop="unitPrice" key="unitPrice" label="单价" align="right" show-overflow-tooltip min-width="80" />
        <el-table-column prop="totalPrice" key="totalPrice" label="总价" align="right" show-overflow-tooltip min-width="120" />
        <el-table-column prop="auditTime" key="auditTime" label="发运日期" align="right" show-overflow-tooltip min-width="150">
          <template #default="{row}">
            <span>{{ parseTime(row.auditTime,'{y}-{m}-{d}') }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="auditUserName" key="auditUserName" label="发运人" align="right" show-overflow-tooltip min-width="120" />
        <el-table-column prop="actualUserName" key="actualUserName" label="过磅人" align="right" show-overflow-tooltip min-width="120" />
        <el-table-column prop="supplierName" key="supplierName" label="物流公司" align="center" show-overflow-tooltip min-width="180" />
        <el-table-column prop="cargoSerialNumber" key="cargoSerialNumber" label="车次" align="center" show-overflow-tooltip min-width="120" />
        <el-table-column prop="licensePlate" key="licensePlate" label="车牌" align="center" show-overflow-tooltip min-width="150" />
        <el-table-column prop="workshopNames" key="workshopNames" label="生产部门" align="center" show-overflow-tooltip min-width="180" />
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
import { shipRecord } from '@/api/contract/sales-manage/order-tracking'
import { ref, defineEmits, defineProps, watch, computed, inject } from 'vue'
import { parseTime } from '@/utils/date'

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

const list = ref([])
const tableLoading = ref(false)

const emit = defineEmits(['update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const permission = inject('permission')

const params = computed(() => {
  return {
    projectId: props.detailRow.id
  }
})

watch(
  () => visible.value,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

// 列格式转换
const columnsDataFormat = [
  ['unitPrice', 'to-thousand'],
  ['totalPrice', 'to-thousand']
]

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.happened-record-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    paginate: true
  },
  visible
)

// 获取发运记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
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
