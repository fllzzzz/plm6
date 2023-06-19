<template>
  <common-drawer
    ref="drawerRef"
    title="仓库名称"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="delivery-detail"
    size="900px"
  >
    <template #titleRight>
      <common-button size="mini" type="primary" @click="formVisible=true">新增</common-button>
    </template>
    <template #content>
      <common-table :data="workshopName" v-loading="!loaded" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="type" prop="type" label="类型" align="center">
          <template v-slot="scope">
            <span>{{scope.row.workshopId?'车间':'普通'}}</span>
          </template>
        </el-table-column>
        <el-table-column key="name" prop="name" label="名称" align="center" />
        <el-table-column label="操作" width="130px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button icon="el-icon-delete" type="danger" size="mini" @click="deleteItem(row)" :disabled="submitLoading" />
        </template>
      </el-table-column>
      </common-table>
      <workshopForm v-model="formVisible" />
    </template>
  </common-drawer>
</template>

<script setup>
import { delWorkshop } from '@/api/config/wms/warehouse'
import { ref, defineEmits, defineProps, watch } from 'vue'

import useWorkshopName from '@compos/store/use-workshop-name'
import { useStore } from 'vuex'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { ElNotification } from 'element-plus'
import workshopForm from './workshop-form'

const emit = defineEmits(['update:modelValue', 'success'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const formVisible = ref(false)
const submitLoading = ref(false)

watch(
  visible,
  (val) => {
    // if (val) {
    //   fetchList()
    // }
  }
)

const { loaded, workshopName } = useWorkshopName()

const store = useStore()
const drawerRef = ref()

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.delivery-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

function handleSuccess() {
  store.dispatch('config/fetchWorkshopName')
}

async function deleteItem(val) {
  submitLoading.value = true
  try {
    await delWorkshop(val.id)
    ElNotification({ title: '删除成功', type: 'success' })
    handleSuccess()
  } catch (error) {
    console.log('审核', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
