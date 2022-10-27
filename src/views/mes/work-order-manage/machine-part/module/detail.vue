<template>
  <common-drawer ref="drawerRef" title="零件详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="60%">
    <template #content>
      <!--表格渲染-->
      <common-table ref="table" :data="partData" empty-text="暂无数据" :max-height="maxHeight" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" min-width="60" type="index" />
        <el-table-column prop="serialNumber" key="serialNumber" label="编号" align="center" />
        <el-table-column prop="specification" key="specification" label="规格" align="center" />
        <el-table-column prop="length" key="length" label="长度（mm）" align="center" />
        <el-table-column prop="material" key="material" label="材质" align="center" />
        <el-table-column prop="quantity" key="quantity" label="数量" align="center" />
        <el-table-column prop="weight" key="weight" label="重量（kg）" align="center" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { defineProps, defineEmits, ref } from 'vue'
import { partDetail } from '@/api/mes/work-order-manage/machine-part'

const emit = defineEmits(['update:visible'])
const drawerRef = ref()
const partData = ref([]) // 零件详情数据

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object,
    default: () => {}
  }
})

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: partDetailGet })

async function partDetailGet() {
  try {
    const data = await partDetail({ orderId: props.detailData.orderId })
    partData.value = data
  } catch (error) {
    console.log('获取零件详情失败', error)
  }
}
</script>

<style lang="scss" scoped>
</style>

