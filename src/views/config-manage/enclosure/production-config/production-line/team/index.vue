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
        <el-table-column key="leaderName" prop="leaderName" label="组长" min-width="80px" show-overflow-tooltip />
        <el-table-column key="memberNames" prop="memberNames" label="组员" min-width="160px" show-overflow-tooltip />
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
          :options="productionTeamOptions"
          :type="'other'"
          multiple
          filterable
          clearable
          :dataStructure="{ key: 'id', label: 'label', value: 'id' }"
          placeholder="请选择班组"
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
import { get, productionTeam, saveProductionTeam } from '@/api/config/enclosure/production-config/production-line-team'
import { defineProps, defineExpose, ref, watch, computed, inject, nextTick } from 'vue'

import { ElNotification } from 'element-plus'

const maxHeight = inject('maxHeight')

const selectValue = ref([])
const dialogVisible = ref(false)
const submitLoading = ref(false)
const listLoading = ref(false)
const productionTeamLoading = ref(false)
const productionTeamOptions = ref([])
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

// 获取生产组列表
async function fetchList() {
  try {
    list.value = []
    listLoading.value = true
    const data = await get({ id: lineId.value })
    list.value = (data.teams || []).map((row) => {
      const members = []
      row.userLinkList.forEach((m) => {
        if (m.boolLeaderEnum) {
          row.leaderName = m.userName
          row.leaderId = m.userId
          row.teamId = m.teamId
          row.leader = {
            id: m.userId,
            name: m.userName
          }
        } else {
          members.push({
            teamId: m.teamId,
            id: m.userId,
            name: m.userName
          })
        }
      })
      row.members = members
      if (row.members.length > 0) {
        row.memberNames = row.members.map((row) => row.name).join(', ')
        row.memberIds = row.members.map((row) => row.id)
      } else {
        row.memberNames = ''
        row.memberIds = []
      }
      return row
    })
  } catch (error) {
    console.log('获取生产班组列表', error)
  } finally {
    listLoading.value = false
  }
}

// 获取生产线下所有生产组
async function fetchProductionTeam() {
  try {
    productionTeamOptions.value = []
    productionTeamLoading.value = true
    const { content } = await productionTeam({ factoryId: props.line?.factoryId })
    productionTeamOptions.value = content.map(row => {
      row.leaderName = ''
      row.userLinkList.forEach(m => {
        if (m.boolLeaderEnum) {
          row.leaderName = m.userName
        }
      })
      row.label = row.leaderName + ' - ' + row.processName
      return row
    })
    selectValue.value = list.value.map(v => v.teamId)
  } catch (error) {
    console.log('获取生产线下所有生产组', error)
  } finally {
    productionTeamLoading.value = false
  }
}

// 保存生产组人员
async function submitIt() {
  try {
    submitLoading.value = true
    await saveProductionTeam({
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
