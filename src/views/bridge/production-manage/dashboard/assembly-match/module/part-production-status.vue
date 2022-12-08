<template>
  <common-drawer
    ref="drawerRef"
    title="零件生产状态"
    v-model="drawerVisible"
    :contentLoading="tableLoading"
    direction="rtl"
    :before-close="handleClose"
    size="50%"
  >
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="permission.print"
          api-key="mesMachinePartDetail"
          :params="{ ids: props.ids }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
    </template>
    <template #content>
      <div class="nameViews">
        <span>构件：</span>
        <span v-for="n in names" :key="n.name">
          <el-tag :type="n.tagType" effect="plain" style="margin-right:5px;margin-bottom:5px;">{{ n.name }}</el-tag>
        </span>
      </div>
      <common-table :data="list" :max-height="maxHeight" style="width: 100%;margin-top:10px;">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="140">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="140">
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="needQuantity" prop="needQuantity" :show-overflow-tooltip="true" label="所需数量" min-width="100">
          <template v-slot="scope">
            <span>{{ scope.row.needQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="usableQuantity" prop="usableQuantity" :show-overflow-tooltip="true" label="可使用" min-width="100">
          <template v-slot="scope">
            <span>{{ scope.row.usableQuantity }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/bridge/bridge-production-manage/assembly-match'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  ids: {
    type: Array,
    default: () => []
  },
  names: {
    type: Array,
    default: () => []
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.nameViews'],
    extraHeight: 10,
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    }
  },
  { immediate: true }
)

const tableLoading = ref(false)
const list = ref([])
const permission = inject('permission')

async function fetchList() {
  try {
    tableLoading.value = true
    const data = await detail({ ids: props.ids })
    list.value = data
  } catch (error) {
    console.log('获取零件生产状态', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
