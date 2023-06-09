<template>
  <common-drawer
    ref="drawerRef"
    :title="`${processInfo.name} 工序在制品统计`"
    v-model="upsDrawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="70%"
  >
    <template #titleAfter>
      <el-tag size="small" effect="plain">
        项目：<span>{{ processData.project?.serialNumber }}-{{ processData.project?.name }}</span>
      </el-tag>
    </template>
    <template #titleRight>
      <print-table
        v-permission="permission.print"
        api-key="mesProcessStatisticsList"
        :params="{ ...queryParams }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
    <template #content>
      <common-table
        v-loading="tableLoading"
        :data="list"
        :show-empty-symbol="false"
        :max-height="maxHeight - 100"
        row-key="rowId"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="monomer.name" :show-overflow-tooltip="true" label="单体">
          <template #default="{ row }">
            <span>{{ row.monomer ? row.monomer?.name : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="area.name" :show-overflow-tooltip="true" label="区域">
          <template #default="{ row }">
            <span>{{ row.area ? row.area?.name : '/' }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="name" :show-overflow-tooltip="true" label="名称" />
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" />
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="quantity" label="数量" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="netWeight" label="单净重（kg）" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="grossWeight" label="单毛重（kg）" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="totalNetWeight" label="总净重（kg）" />
        <el-table-column :show-overflow-tooltip="true" align="center" prop="totalGrossWeight" label="总毛重（kg）" />
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
import { getProcess } from '@/api/mes/factory-report/product-statistics.js'
import { defineProps, defineEmits, ref, inject, computed } from 'vue'
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
  processInfo: {
    type: Object
  },
  processData: {
    type: Object
  }
})

const permission = inject('permission')
const { visible: upsDrawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchList })

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
    projectId: props.processData.project?.id,
    processId: props.processInfo?.id,
    taskTypeEnum: props.processInfo?.productType
  }
})

async function fetchList() {
  try {
    list.value = []
    tableLoading.value = true
    const { content = [], totalElements } = await getProcess({
      ...queryParams.value,
      ...queryPage
    })
    list.value = content.map((v, i) => {
      v.rowId = i + '' + Math.random()
      return v
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取工序在制品统计详情失败', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
