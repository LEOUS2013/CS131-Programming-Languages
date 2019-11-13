import java.util.concurrent.atomic.AtomicIntegerArray;
class AcmeSafe implements State {
    private AtomicIntegerArray value;
    private byte maxval;
    
    AcmeSafe(byte[] v) 
    { 
    	value = new AtomicIntegerArray(v.length);
    	for(int i = 0; i < v.length; i++)
    	{
    		value.set(i, v[i]);
    	}

    	maxval = 127; 
    }

    AcmeSafe(byte[] v, byte m) 
    { 
    	value = new AtomicIntegerArray(v.length);
    	for(int i = 0; i < v.length; i++)
    	{
    		value.set(i, v[i]);
    	}
    	maxval = m; 
    }

    public int size() 
    { 
    	return value.length(); 
    }

    public byte[] current() 
    { 
    	byte[] bytearr = new byte[value.length()];
    	for(int i = 0; i < value.length(); i++)
    	{
    		bytearr[i] = (byte)value.get(i);
    	}
    	return bytearr;
    }

    public boolean swap(int i, int j) 
    {
		if (value.get(i) <= 0 || value.get(j) >= maxval) 
		{
		    return false;
		}
		
		value.getAndIncrement(j);
		value.getAndDecrement(i);
		return true;
    }
}