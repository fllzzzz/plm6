<template>
  <div>
    <div style="margin-bottom:10px;">
      <el-input
        v-model="serialNumber"
        size="small"
        placeholder="编号搜索"
        style="width: 170px;"
        class="filter-item"
      />
    </div>
    <common-table :data="list" v-loading="tableLoading" :max-height="maxHeight">
      <el-table-column key="monomer.name" prop="monomer.name" label="所选编号" align="center" />
      <!--编辑与删除-->
      <el-table-column
        label="操作"
        width="80px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button
            size="mini"
            icon="el-icon-delete"
            type="danger"
          />
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { deliveryProductList } from '@/api/project-manage/delivery-manage/homemade-delivery'
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
const serialNumber = ref()

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
