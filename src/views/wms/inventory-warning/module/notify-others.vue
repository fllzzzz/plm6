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
      <common-button v-permission="permission.edit" :loading="submitLoading" type="primary" size="mini" @click="submit">
        提 交
      </common-button>
    </template>
    <template #content>
      <common-table
        v-loading="!loaded || configLoading"
        :data="form.list"
        return-source-data
        :show-empty-symbol="false"
        style="width: 100%"
        row-key="factoryId"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="factoryName" label="工厂名称" :show-overflow-tooltip="true" width="150" />
        <el-table-column prop="createTime" label="通知人" :show-overflow-tooltip="true" min-width="180">
          <template v-slot="scope">
            <user-dept-cascader
              v-model="scope.row.userIds"
              filterable
              :collapse-tags="false"
              :disabled="!checkPermission(permission.edit)"
              multiple
              clearable
              show-all-levels
              style="width: 100%"
              placeholder="选择通知人"
            />
          </template>
        </el-table-column>
        <el-table-column prop="createTime" label="通知部门" :show-overflow-tooltip="true" min-width="180">
          <template v-slot="scope">
            <dept-cascader
              v-model="scope.row.deptIds"
              filterable
              :collapse-tags="false"
              :disabled="!checkPermission(permission.edit)"
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
// TODO: disabled 改为 span, 直接显示用户名，用户信息从store中读取（暂未封装）
import { getInventoryNotifyConf, setInventoryNotifyConf } from '@/api/wms/inventory-warning'
import { inject, defineEmits, defineProps, ref } from 'vue'
import { isNotBlank } from '@/utils/data-type'
import { arr2obj } from '@/utils/convert/type'
import checkPermission from '@/utils/system/check-permission'

import useVisible from '@compos/use-visible'
import useFactory from '@compos/store/use-factories'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import deptCascader from '@comp-base/dept-cascader.vue'
import { ElNotification } from 'element-plus'

const emit = defineEmits(['success', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const permission = inject('permission')

const form = ref({
  list: []
})

const submitLoading = ref(false)
const configLoading = ref(false)

const { loaded, factories } = useFactory(formInit)

const { visible, handleClose } = useVisible({ emit, props })

// 表单初始化
async function formInit() {
  try {
    form.value.list = []
    if (isNotBlank(factories.value)) {
      configLoading.value = true
      const config = await fetchConf()
      factories.value.forEach((f) => {
        const conf = config[f.id] || {}
        form.value.list.push({
          factoryId: f.id,
          factoryName: f.name,
          userIds: conf.userIds || [],
          deptIds: conf.deptIds || []
        })
      })
    }
  } catch (error) {
    console.log('数据转换', error)
  } finally {
    configLoading.value = false
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
