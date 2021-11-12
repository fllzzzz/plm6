<template>
  <common-drawer
    ref="drawerRef"
    v-model="visible"
    direction="rtl"
    :size="1200"
    title="预警通知人"
    :wrapper-closable="false"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" type="primary" size="mini" @click="submit"> 提 交 </common-button>
    </template>
    <template #content>
      <common-table v-loading="!loaded" :data="form.list" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="factoryName" label="工厂名称" :show-overflow-tooltip="true" width="150" />
        <el-table-column prop="createTime" label="通知人" :show-overflow-tooltip="true" min-width="180">
          <!-- <template v-slot="scope">
              <user-dept-cascader
                v-if="scope.row.userOrDept === userOrDeptEnum.USER.V"
                :filter-not-dd-user="false"
                :filterable="true"
                :collapse-tags="false"
                multiple
                clearable
                show-all-levels
                style="width: 100%"
                placeholder="选择通知人"
              />
            </template> -->
        </el-table-column>
        <el-table-column prop="createTime" label="通知部门" :show-overflow-tooltip="true" min-width="180">
          <template v-slot="scope">
            <dept-cascader
              v-model="scope.row.deptIds"
              filterable
              :collapse-tags="false"
              multiple
              clearable
              show-all-levels
              style="width: 100%"
              placeholder="选择通知部门"
            />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getInventoryNotifyConf, setInventoryNotifyConf } from '@/api/wms/inventory-warning'
import { defineEmits, defineProps, watch, ref } from 'vue'
import { isNotBlank } from '@/utils/data-type'
import { arr2obj } from '@/utils/convert/type'

import useVisible from '@compos/use-visible'
import useFactory from '@compos/store/use-factories'
// import userDeptCascader from '@/views/components/base/user-dept-cascader'
import deptCascader from '@comp-base/dept-cascader.vue'
import { ElNotification } from 'element-plus'

const emit = defineEmits(['success', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const form = ref({
  list: []
})

const submitLoading = ref(false)

const { loaded, factories } = useFactory()

const { visible, handleClose } = useVisible({ emit, props })

watch(
  factories,
  (val) => {
    formInit(val)
  },
  { immediate: true }
)

// 表单初始化
async function formInit(factoryList) {
  form.value.list = []
  if (isNotBlank(factoryList)) {
    console.log('factoryList', factoryList)
    const config = await fetchConf()
    factoryList.forEach((f) => {
      const conf = config[f.id] || {}
      form.value.list.push({
        factoryId: f.id,
        factoryName: f.name,
        userIds: conf.userIds || [],
        deptIds: conf.deptIds || []
      })
    })
  }
}

// 加载预警通知配置
async function fetchConf() {
  let config = {}
  try {
    const { content } = await getInventoryNotifyConf()
    config = arr2obj(content, 'factoryId')
  } catch (error) {
    console.log('配置信息', error)
  }
  return config
}

async function submit() {
  submitLoading.value = true
  try {
    await setInventoryNotifyConf(form.value.list)
    ElNotification({ title: '修改成功', type: 'success' })
    handleClose()
  } catch (error) {
    console.log('保存预警通知配置', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
