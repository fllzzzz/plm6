<template>
  <common-drawer ref="drawerRef" title="排产记录" v-model="taskDrawerVisible" direction="rtl" :before-close="handleClose" size="70%">
    <template #titleAfter>
      <el-tag size="small" effect="plain">
        项目：<span>{{ taskInfo.project?.serialNumber }}-{{ taskInfo.project?.name }}</span>
      </el-tag>
    </template>
    <template #titleRight>
      <print-table v-permission="permission.print" api-key="mesTaskStatisticsList" :params="{ ...queryParams }" size="mini" type="warning" class="filter-item" />
    </template>
    <template #content>
      <common-table
        v-loading="tableLoading"
        :data="list"
        :max-height="maxHeight - 100"
        row-key="rowId"
        style="width: 100%"
        show-summary
        :summary-method="getSummaries"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体" />
        <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域" />
        <el-table-column prop="name" :show-overflow-tooltip="true" label="名称" />
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" />
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="quantity" label="数量" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="netWeight" label="单重（kg）" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="totalNetWeight" label="总重（kg）" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="completeTime" label="排产日期" width="120">
          <template #default="{ row }">
            <span>{{ parseTime(row.completeTime, '{y}-{m}-{d}') }}</span>
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
import { getTask } from '@/api/mes/task-tracking/wip-statistics.js'
import { defineProps, defineEmits, ref, inject, computed } from 'vue'
import { parseTime } from '@/utils/date'
import { tableSummary } from '@/utils/el-extra'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  taskInfo: {
    type: Object
  }
})

const permission = inject('permission')
const { visible: taskDrawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const tableLoading = ref(false)
const list = ref([])
const queryParams = computed(() => {
  return {
    projectId: props.taskInfo.project?.id
  }
})

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'totalNetWeight']
  })
}

async function fetchList() {
  try {
    list.value = []
    tableLoading.value = true
    const { content = [], totalElements } = await getTask({
      ...queryParams.value,
      ...queryPage
    })
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取排产记录记录详情失败', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
