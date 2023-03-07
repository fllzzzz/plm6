<template>
  <common-drawer
    ref="drawerRef"
    title="未齐套清单"
    v-model="unCompleteDrawerVisible"
    :contentLoading="tableLoading"
    direction="rtl"
    :before-close="handleClose"
    size="70%"
  >
    <template #titleRight>
      <div class="print-wrap">
        <print-table :api-key="apiKey" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
      </div>
    </template>
    <template #content>
      <common-table :data="list" :max-height="maxHeight" style="width: 100%; margin-top: 10px">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column label="属性" prop="type" align="center">
          <template #default="{ row }">
            <el-tag :type="row.type === componentTypeEnum.MACHINE_PART.V ? 'warning' : 'success'">{{
              componentTypeEnum.VL[row.type]
            }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" />
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="140" />
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度" />
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" />
        <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" />
        <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" />
        <el-table-column key="completeQuantity" prop="completeQuantity" :show-overflow-tooltip="true" label="实际生产数">
          <template v-slot="scope">
            <span :style="scope.row.completeQuantity===scope.row.quantity?'color: green':'color: red'">{{ scope.row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="groups.name" prop="groups.name" :show-overflow-tooltip="true" label="负责班组" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
// import { detail } from '@/api/mes/production-manage/dashboard/assembly-match'
import { defineProps, defineEmits, ref } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: unCompleteDrawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

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

// watch(
//   () => props.visible,
//   (visible) => {
//     if (visible) {
//       fetchList()
//     }
//   },
//   { immediate: true }
// )

const tableLoading = ref(false)
const list = ref([])

// async function fetchList() {
//   try {
//     tableLoading.value = true
//     const data = await detail({ ids: props.ids })
//     list.value = data
//   } catch (error) {
//     console.log('获取未齐套零件数据', error)
//   } finally {
//     tableLoading.value = false
//   }
// }
</script>
