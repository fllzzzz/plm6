<template>
  <div>
    <common-table :data="list" v-loading="tableLoading" :max-height="maxHeight" style="width:300px;">
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column key="monomer.name" prop="monomer.name" label="构件类型" align="center" />
      <el-table-column key="area.name" prop="area.name" label="数量(件)" align="center" />
    </common-table>
  </div>
</template>

<script setup>
import { ref, defineEmits, defineProps } from 'vue'

const emit = defineEmits(['update:modelValue', 'success'])

const props = defineProps({
  query: {
    type: Object,
    default: () => {}
  },
  maxHeight: {
    type: Number,
    default: undefined
  }
})

const list = ref([])
const tableLoading = ref(false)

// 获取收货明细
async function fetchList() {
  // let _list = []
  // tableLoading.value = true
  // try {
  //   const { content = [] } = await deliveryProductList(props.detailInfo?.id)
  //   _list = content
  // } catch (error) {
  //   console.log('收货明细', error)
  // } finally {
  //   list.value = _list
  //   tableLoading.value = false
  // }
}
</script>
