<template>
  <el-select
    v-model="selectIds"
    :size="size"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="loading"
    filterable
    :placeholder="placeholder"
    @change="handleChange"
  >
    <el-option-group v-for="group in options" :key="group.label" :label="group.label">
      <el-option
        v-for="item in group.options"
        :key="item.id"
        :label="item.label"
        :disabled="disabledValue.includes(item.id)"
        :value="item.id"
      />
    </el-option-group>
  </el-select>
</template>

<script setup>
import { getUserAllSimple as getAllUser } from '@/api/common'
import { defineExpose, defineProps, defineEmits, ref, watch } from 'vue'

const emit = defineEmits(['change', 'update:value'])

const props = defineProps({
  placeholder: {
    type: String,
    default: '请选择人员'
  },
  multiple: {
    type: Boolean,
    default: false
  },
  collapseTags: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: false
  },
  disabledValue: {
    type: Array,
    default: () => []
  },
  size: {
    type: String,
    default: 'small'
  },
  // eslint-disable-next-line vue/require-default-prop
  value: {
    type: [Number, Array]
  }
})

const loading = ref(false)
const selectIds = ref([])
const options = ref([])
const sourceOptions = ref([])

watch(
  () => props.value,
  (val) => {
    selectIds.value = val
    handleChange(val)
  },
  { immediate: true }
)

function handleChange(val) {
  emit('update:value', val)
  const userList = getUser(val)
  emit('change', userList)
}

function getUser(val) {
  if (val instanceof Array) {
    const userList = new Array(3)
    const _userIdList = [...val]
    sourceOptions.value.forEach((user) => {
      const index = _userIdList.indexOf(user.id)
      if (index > -1) {
        userList[index] = JSON.parse(JSON.stringify(user))
      }
    })
    return userList
  } else {
    let user
    for (const u of sourceOptions.value) {
      if (u.id === val) {
        user = JSON.parse(JSON.stringify(u))
        break
      }
    }
    return user
  }
}

async function fetchAllUser() {
  let _options = []
  try {
    loading.value = true
    const { content } = await getAllUser()
    sourceOptions.value = content
    _options = struTransform(content)
  } catch (error) {
    console.log('获取所有用户-下拉列表', error)
  } finally {
    options.value = _options
    loading.value = false
  }
}

function struTransform(data) {
  const cascade = []
  const dept = []
  data.forEach((user) => {
    const userDept = user.deptName
    if (dept.indexOf(userDept) === -1) {
      dept.push(userDept)
      cascade.push({
        label: userDept,
        options: []
      })
    }
    const newUser = {
      id: user.id,
      label: user.name
    }
    cascade[dept.indexOf(userDept)].options.push(newUser)
  })
  return cascade || []
}

fetchAllUser()

defineExpose({
  getUser
})
</script>

<style lang="scss" scoped>
//  /deep/.el-input__inner{
//      height: 30.5px!important;
//  }
</style>
