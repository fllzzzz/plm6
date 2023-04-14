<template>
  <common-drawer
    ref="dialogRef"
    title="计划"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="80%"
  >
    <template #titleAfter>
      <span>项目:{{detailInfo.project}}</span>
    </template>
    <template #content>
      <div class="wrap">
        <div class="wrap-left">
          <common-table :data="list" highlight-current-row @current-change="handleCurrentChange">
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column prop="projectContent" label="围护种类" align="center" width="100" show-overflow-tooltip />
            <el-table-column prop="unit" label="结算单位" align="center" min-width="120" show-overflow-tooltip />
            <el-table-column prop="quantityWork" label="合同量" align="center" width="110" show-overflow-tooltip />
          </common-table>
        </div>
        <div class="wrap-right">
          <el-tag v-if="!currentRow?.id" type="info" size="medium"> * 请点击左侧项目列表查看详情 </el-tag>
          <template v-else>
            <planForm :visibleValue="visible" :currentRow="currentRow"/>
          </template>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { enclosureProjectContent } from '@/api/enclosure/enclosure-plan/area'
import { ref, defineEmits, defineProps, watch } from 'vue'

import useVisible from '@/composables/use-visible'

import planForm from './plan-form/index'

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })

watch(
  visible,
  (val) => {
    if (val) {
      currentRow.value = {}
      fetchList()
    }
  }
)

const list = ref([])
const dialogRef = ref()
const tableLoading = ref(false)
const currentRow = ref({})

function handleCurrentChange(val) {
  currentRow.value = val
}

// 获取项目内容及合同量
async function fetchList() {
  let _list = []
  if (!props.detailInfo?.projectId) {
    return
  }
  tableLoading.value = true
  try {
    const data = await enclosureProjectContent(props.detailInfo.projectId)
    _list = data
  } catch (error) {
    console.log('获取项目内容及合同量失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
<style lang="scss" scoped>
.wrap {
  display:flex;
  .wrap-right{
    flex:1;
    padding-left:20px;
  }
}
</style>
