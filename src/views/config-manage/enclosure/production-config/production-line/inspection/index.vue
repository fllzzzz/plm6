<template>
  <div>
    <div v-show="!lineId">
      <div class="my-code">点击生产线查看详情</div>
    </div>
    <div v-show="lineId">
      <!--表格渲染-->
      <common-table ref="tableRef" :data="list" :max-height="maxHeight + 93">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="processName" prop="processName" label="工序名称" min-width="80px" show-overflow-tooltip />
        <el-table-column key="memberNames" prop="memberNames" label="质检人员" min-width="160px" show-overflow-tooltip />
      </common-table>
      <common-dialog
        title="选择班组"
        v-model="dialogVisible"
        :before-close="
          () => {
            dialogVisible = false
          }
        "
        :close-on-click-modal="false"
        width="500px"
      >
        <template #titleRight>
          <common-button :loading="submitLoading" size="mini" type="primary" @click="submitIt"> 保存 </common-button>
        </template>
        <common-select
          v-model="selectValue"
          v-loading="productionTeamLoading"
          :options="inspectionTeamOptions"
          :type="'other'"
          multiple
          filterable
          clearable
          :dataStructure="{ key: 'id', label: 'label', value: 'id' }"
          placeholder="请选择质检人员"
          style="width: 100%"
        >
          <template #empty>
            <div style="text-align: center; display: flex; flex-direction: column; padding: 10px; color: #c0c4cc">
              <span>暂无数据</span>
              <span style="margin-top: 5px; color: #f56c6c">*请到班组管理进行配置</span>
            </div>
          </template>
        </common-select>
      </common-dialog>
    </div>
  </div>
</template>

<script setup>
import { get, inspectionTeam, saveInspectionTeam } from '@/api/config/enclosure/production-config/production-line-team'
import { defineProps, defineExpose, ref, watch, computed, inject, nextTick } from 'vue'

import { ElNotification } from 'element-plus'

const maxHeight = inject('maxHeight')

const selectValue = ref([])
const dialogVisible = ref(false)
const submitLoading = ref(false)
const listLoading = ref(false)
const productionTeamLoading = ref(false)
const inspectionTeamOptions = ref([])
const list = ref([])

const props = defineProps({
  line: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => dialogVisible.value,
  (val) => {
    if (val) {
      nextTick(() => {
        fetchProductionTeam()
      })
    }
  }
)

const lineId = computed(() => {
  return props.line && props.line.id
})

watch(
  () => lineId.value,
  (id) => {
    if (id) {
      fetchList()
    }
  },
  { immediate: true }
)

// 获取质检组列表
async function fetchList() {
  try {
    list.value = []
    listLoading.value = true
    const data = await get({ id: lineId.value })
    list.value = (data.inspectionTeams || []).map((row) => {
      row.memberNames = row.userLinkList.map(m => {
        row.teamId = m.teamId
        return m.userName
      }).join(', ')
      return row
    })
  } catch (error) {
    console.log('获取质检班组列表', error)
  } finally {
    listLoading.value = false
  }
}

// 获取生产线下所有质检组
async function fetchProductionTeam() {
  try {
    inspectionTeamOptions.value = []
    productionTeamLoading.value = true
    const { content } = await inspectionTeam({ factoryId: props.line?.factoryId })
    inspectionTeamOptions.value = content.map(row => {
      row.label = row.processName + ' - ' + row.userLinkList.map(v => v.userName).join(', ')
      return row
    })
    selectValue.value = list.value.map(v => v.teamId)
  } catch (error) {
    console.log('获取生产线下所有质检组', error)
  } finally {
    productionTeamLoading.value = false
  }
}

// 保存质检组人员
async function submitIt() {
  try {
    submitLoading.value = true
    await saveInspectionTeam({
      id: lineId.value,
      teamIds: selectValue.value
    })
    ElNotification({
      title: '班组绑定成功',
      type: 'success',
      duration: 2500
    })
    dialogVisible.value = false
    fetchList()
  } catch (error) {
    console.log(error, '绑定班组')
  } finally {
    submitLoading.value = false
  }
}

defineExpose({
  toAdd: () => (dialogVisible.value = true)
})
</script>
